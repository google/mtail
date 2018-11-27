// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

// mtail programs may be updated while mtail is running, and they will be
// reloaded without having to restart the mtail process. Programs can be
// created and deleted as well, and some configuration systems do an atomic
// rename of the program when it is installed, so mtail is also aware of file
// moves.  The Master Control Program is responsible for managing the lifetime
// of mtail programs.

import (
	"expvar"
	"html/template"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/pkg/errors"
	"github.com/spf13/afero"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/watcher"
)

var (
	// LineCount counts the number of lines read by the program loader from the input channel.
	LineCount = expvar.NewInt("line_count")
	// ProgLoads counts the number of program load events.
	ProgLoads = expvar.NewMap("prog_loads_total")
	// ProgLoadErrors counts the number of program load errors.
	ProgLoadErrors    = expvar.NewMap("prog_load_errors")
	progRuntimeErrors = expvar.NewMap("prog_runtime_errors")
)

const (
	fileExt = ".mtail"
)

// LoadAllPrograms loads all programs in a directory and starts watching the
// directory for filesystem changes.  Any compile errors are stored for later retrieival.
// This function returns an error if an internal error occurs.
func (l *MasterControl) LoadAllPrograms() error {
	s, err := os.Stat(l.programPath)
	if err != nil {
		return errors.Wrapf(err, "failed to stat %q", l.programPath)
	}
	if err = l.w.Add(l.programPath, l.eventsHandle); err != nil {
		glog.Infof("Failed to add watch on %q but continuing: %s", l.programPath, err)
	}
	switch {
	case s.IsDir():
		fis, rerr := ioutil.ReadDir(l.programPath)
		if rerr != nil {
			return errors.Wrapf(rerr, "Failed to list programs in %q", l.programPath)
		}

		for _, fi := range fis {
			if fi.IsDir() {
				continue
			}
			err = l.LoadProgram(path.Join(l.programPath, fi.Name()))
			if err != nil {
				if l.errorsAbort {
					return err
				}
				glog.Warning(err)
			}
		}
	default:
		err = l.LoadProgram(l.programPath)
		if err != nil {
			if l.errorsAbort {
				return err
			}
			glog.Warning(err)
		}
	}
	return nil
}

// LoadProgram loads or reloads a program from the path specified.  The name of
// the program is the basename of the file.
func (l *MasterControl) LoadProgram(programPath string) error {
	name := filepath.Base(programPath)
	if strings.HasPrefix(name, ".") {
		glog.V(2).Infof("Skipping %s because it is a hidden file.", programPath)
		return nil
	}
	if filepath.Ext(name) != fileExt {
		glog.V(2).Infof("Skipping %s due to file extension.", programPath)
		return nil
	}
	f, err := l.fs.Open(programPath)
	if err != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Wrapf(err, "Failed to read program %q", programPath)
	}
	defer func() {
		if err := f.Close(); err != nil {
			glog.Warning(err)
		}
	}()
	l.programErrorMu.Lock()
	defer l.programErrorMu.Unlock()
	l.programErrors[name] = l.CompileAndRun(name, f)
	if l.programErrors[name] != nil {
		if l.errorsAbort {
			return l.programErrors[name]
		}
		glog.Infof("Compile errors for %s:\n%s", name, l.programErrors[name])
	}
	return nil
}

const loaderTemplate = `
<h2 id="loader">Program Loader</h2>
<table border=1>
<tr>
<th>program name</th>
<th>errors</th>
<th>load errors</th>
<th>load successes</th>
<th>runtime errors</th>
</tr>
<tr>
{{range $name, $errors := $.Errors}}
<td>{{$name}}</td>
<td>
{{if $errors}}
{{$errors}}
{{else}}
No compile errors
{{end}}
</td>
<td>{{index $.Loaderrors $name}}</td>
<td>{{index $.Loadsuccess $name}}</td>
<td>{{index $.RuntimeErrors $name}}</td>
</tr>
{{end}}
</table>
`

// WriteStatusHTML writes the current state of the loader as HTML to the given writer w.
func (l *MasterControl) WriteStatusHTML(w io.Writer) error {
	t, err := template.New("loader").Parse(loaderTemplate)
	if err != nil {
		return err
	}
	l.programErrorMu.RLock()
	defer l.programErrorMu.RUnlock()
	data := struct {
		Errors        map[string]error
		Loaderrors    map[string]string
		Loadsuccess   map[string]string
		RuntimeErrors map[string]string
	}{
		l.programErrors,
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
	}
	for name := range l.programErrors {
		if ProgLoadErrors.Get(name) != nil {
			data.Loaderrors[name] = ProgLoadErrors.Get(name).String()
		}
		if ProgLoads.Get(name) != nil {
			data.Loadsuccess[name] = ProgLoads.Get(name).String()
		}
		if progRuntimeErrors.Get(name) != nil {
			data.RuntimeErrors[name] = progRuntimeErrors.Get(name).String()
		}
	}
	return t.Execute(w, data)
}

// CompileAndRun compiles a program read from the input, starting execution if
// it succeeds.  If an existing virtual machine of the same name already
// exists, the previous virtual machine is terminated and the new loaded over
// it.  If the new program fails to compile, any existing virtual machine with
// the same name remains running.
func (l *MasterControl) CompileAndRun(name string, input io.Reader) error {
	glog.V(2).Infof("CompileAndRun %s", name)
	v, errs := Compile(name, input, l.dumpAst, l.dumpAstTypes, l.syslogUseCurrentYear, l.overrideLocation)
	if errs != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Errorf("compile failed for %s:\n%s", name, errs)
	}
	if v == nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Errorf("Internal error: Compilation failed for %s: No program returned, but no errors.", name)
	}

	if l.dumpBytecode {
		glog.Info("Dumping program objects and bytecode\n", v.DumpByteCode(name))
	}

	// Load the metrics from the compilation into the global metric storage for export.
	for _, m := range v.m {
		if !m.Hidden {
			if l.omitMetricSource {
				m.Source = ""
			}
			err := l.ms.Add(m)
			if err != nil {
				return err
			}
		}
	}

	ProgLoads.Add(name, 1)
	glog.Infof("Loaded program %s", name)

	if l.compileOnly {
		return nil
	}

	l.handleMu.Lock()
	defer l.handleMu.Unlock()

	// Stop any previous VM.
	if handle, ok := l.handles[name]; ok {
		glog.Infof("END OF LINE, %s", name)
		close(handle.lines)
		<-handle.done
		glog.Infof("Stopped %s", name)
	}

	l.handles[name] = &vmHandle{make(chan *logline.LogLine), make(chan struct{})}
	nameCode := nameToCode(name)
	glog.Infof("Program %s has goroutine marker 0x%x", name, nameCode)
	started := make(chan struct{})
	go v.Run(nameCode, l.handles[name].lines, l.handles[name].done, started)
	<-started
	glog.Infof("Started %s", name)

	return nil
}

func nameToCode(name string) uint32 {
	return uint32(name[0])<<24 | uint32(name[1])<<16 | uint32(name[2])<<8 | uint32(name[3])
}

// MasterControl handles the lifecycle of programs and virtual machines, by watching
// the configured program source directory, compiling changes to programs, and
// managing the running virtual machines that receive input from the lines
// channel.
type MasterControl struct {
	ms          *metrics.Store  // pointer to metrics.Store to pass to compiler
	w           watcher.Watcher // watches for program changes
	fs          afero.Fs        // filesystem interface
	programPath string          // Path that contains mtail programs.

	eventsHandle int // record the handle with which to add programs to the watcher

	handleMu sync.RWMutex         // guards accesses to handles
	handles  map[string]*vmHandle // map of program names to virtual machines

	programErrorMu sync.RWMutex     // guards access to programErrors
	programErrors  map[string]error // errors from the last compile attempt of the program

	watcherDone chan struct{} // Synchronise shutdown of the watcher processEvents goroutine
	VMsDone     chan struct{} // Notify mtail when all running VMs are shutdown.

	overrideLocation     *time.Location // Instructs the vm to override the timezone with the specified zone.
	compileOnly          bool           // Only compile programs and report errors, do not load VMs.
	errorsAbort          bool           // Compiler errors abort the loader.
	dumpAst              bool           // print the AST after parse
	dumpAstTypes         bool           // print the AST after type check
	dumpBytecode         bool           // Instructs the loader to dump to stdout the compiled program after compilation.
	syslogUseCurrentYear bool           // Instructs the VM to overwrite zero years with the current year in a strptime instruction.
	omitMetricSource     bool
}

// OverrideLocation sets the timezone location for the VM.
func OverrideLocation(loc *time.Location) func(*MasterControl) error {
	return func(l *MasterControl) error {
		l.overrideLocation = loc
		return nil
	}
}

// CompileOnly sets the Loader to compile programs only, without executing them.
func CompileOnly(l *MasterControl) error {
	l.compileOnly = true
	return ErrorsAbort(l)
}

// ErrorsAbort sets the Loader to abort the Loader on compile errors.
func ErrorsAbort(l *MasterControl) error {
	l.errorsAbort = true
	return nil
}

// DumpAst instructs the Loader to print the AST after program compilation.
func DumpAst(l *MasterControl) error {
	l.dumpAst = true
	return nil
}

// DumpAstTypes instructs the Loader to print the AST after type checking.
func DumpAstTypes(l *MasterControl) error {
	l.dumpAstTypes = true
	return nil
}

// DumpBytecode instructs the loader to print the compiled bytecode after code generation.
func DumpBytecode(l *MasterControl) error {
	l.dumpBytecode = true
	return nil
}

// SyslogUseCurrentYear instructs the VM to annotate yearless timestamps with the current year.
func SyslogUseCurrentYear(l *MasterControl) error {
	l.syslogUseCurrentYear = true
	return nil
}

// OmitMetricSource instructs the Loader to not annotate metrics with their program source when added to the metric store.
func OmitMetricSource(l *MasterControl) error {
	l.omitMetricSource = true
	return nil
}

// NewLoader creates a new program loader that reads programs from programPath.
func NewLoader(programPath string, store *metrics.Store, lines <-chan *logline.LogLine, w watcher.Watcher, fs afero.Fs, options ...func(*MasterControl) error) (*MasterControl, error) {
	if store == nil || lines == nil {
		return nil, errors.New("loader needs a store and lines")
	}
	l := &MasterControl{
		ms:            store,
		fs:            fs,
		w:             w,
		programPath:   programPath,
		handles:       make(map[string]*vmHandle),
		programErrors: make(map[string]error),
		watcherDone:   make(chan struct{}),
		VMsDone:       make(chan struct{}),
	}
	if err := l.SetOption(options...); err != nil {
		return nil, err
	}
	handle, eventsChan := l.w.Events()
	l.eventsHandle = handle
	go l.processEvents(eventsChan)
	go l.processLines(lines)
	return l, nil
}

// SetOption takes one or more option functions and applies them in order to Loader.
func (l *MasterControl) SetOption(options ...func(*MasterControl) error) error {
	for _, option := range options {
		if err := option(l); err != nil {
			return err
		}
	}
	return nil
}

type vmHandle struct {
	lines chan *logline.LogLine
	done  chan struct{}
}

// processEvents manages program lifecycle triggered by events from the
// filesystem watcher.
func (l *MasterControl) processEvents(events <-chan watcher.Event) {
	defer close(l.watcherDone)

	for event := range events {
		switch event.Op {
		case watcher.Delete:
			l.UnloadProgram(event.Pathname)
		case watcher.Update:
			if err := l.LoadProgram(event.Pathname); err != nil {
				glog.Info(err)
			}
		case watcher.Create:
			if err := l.w.Add(event.Pathname, l.eventsHandle); err != nil {
				glog.Info(err)
				continue
			}
			if err := l.LoadProgram(event.Pathname); err != nil {
				glog.Info(err)
			}
		default:
			glog.V(1).Infof("Unexpected event type %+#v", event)
		}
	}
}

// processLines provides fanout of the input log lines to each virtual machine
// running.  Upon close of the incoming lines channel, it also communicates
// shutdown to the target VMs via channel close.  At termination it signals via
// VMsDone that the goroutine has finished, and thus all VMs are terminated.
func (l *MasterControl) processLines(lines <-chan *logline.LogLine) {
	defer close(l.VMsDone)

	// Copy all input LogLines to each VM's LogLine input channel.
	for logline := range lines {
		LineCount.Add(1)
		l.handleMu.RLock()
		for prog := range l.handles {
			l.handles[prog].lines <- logline
		}
		l.handleMu.RUnlock()
	}
	// When lines is closed, the tailer has shut down which signals that it's
	// time to shut down the program loader.
	glog.Info("Shutting down loader.")
	if err := l.w.Close(); err != nil {
		glog.Infof("error closing watcher: %s", err)
	}
	<-l.watcherDone
	l.handleMu.Lock()
	defer l.handleMu.Unlock()
	for prog := range l.handles {
		// Close the per-VM lines channel, and wait for it to signal it's done.
		close(l.handles[prog].lines)
		<-l.handles[prog].done
		delete(l.handles, prog)
	}
}

// UnloadProgram removes the named program from the watcher to prevent future
// updates, and terminates any currently running VM goroutine.
func (l *MasterControl) UnloadProgram(pathname string) {
	if err := l.w.Remove(pathname); err != nil {
		glog.V(2).Infof("Remove watch on %s failed: %s", pathname, err)
	}
	name := filepath.Base(pathname)
	l.handleMu.Lock()
	defer l.handleMu.Unlock()
	if handle, ok := l.handles[name]; ok {
		close(handle.lines)
		<-handle.done
		delete(l.handles, name)
	}
}
