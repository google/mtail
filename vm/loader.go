// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

// mtail programs may be updated while emtail is running, and they will be
// reloaded without having to restart the mtail process. Programs can be
// created and deleted as well, and some configuration systems do an atomic
// rename of the program when it is installed, so mtail is also aware of file
// moves.

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

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/watcher"
)

var (
	// ProgLoads counts the number of program load events.
	ProgLoads = expvar.NewMap("prog_loads_total")
	// ProgLoadErrors counts the number of program load errors.
	ProgLoadErrors = expvar.NewMap("prog_load_errors")
)

var (
	// LineCount counts the number of lines read by the virtual machine engine from the input channel.
	LineCount = expvar.NewInt("line_count")
)

const (
	fileExt = ".mtail"
)

// LoadProgs loads all programs in a directory and starts watching the
// directory for filesystem changes.  Any compile errors are stored for later retrieival.
// This function returns an error if an internal error occurs.
func (l *Loader) LoadProgs(programPath string) error {
	s, err := os.Stat(programPath)
	if err != nil {
		return errors.Wrapf(err, "failed to stat %q", programPath)
	}
	if err = l.w.Add(programPath); err != nil {
		glog.Infof("Failed to add watch on %q but continuing: %s", programPath, err)
	}
	switch {
	case s.IsDir():
		fis, err := ioutil.ReadDir(programPath)
		if err != nil {
			return errors.Wrapf(err, "Failed to list programs in %q", programPath)
		}

		for _, fi := range fis {
			if fi.IsDir() {
				continue
			}
			err = l.LoadProg(path.Join(programPath, fi.Name()))
			if err != nil {
				glog.Warning(err)
				if l.compileOnly {
					return err
				}
			}
		}
	default:
		err = l.LoadProg(programPath)
		if err != nil {
			glog.Warning(err)
			if l.compileOnly {
				return err
			}
		}
	}
	return nil
}

// LoadProg loads or reloads a program from the path specified.  The name of
// the program is the basename of the file.
func (l *Loader) LoadProg(programPath string) error {
	name := filepath.Base(programPath)
	if strings.HasPrefix(name, ".") {
		glog.Infof("Skipping %s because it is a hidden file.", programPath)
		return nil
	}
	if filepath.Ext(name) != fileExt {
		glog.Infof("Skipping %s due to file extension.", programPath)
		return nil
	}
	f, err := l.fs.Open(programPath)
	if err != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Wrapf(err, "Failed to read program %q", programPath)
	}
	defer f.Close()
	l.programErrorMu.Lock()
	defer l.programErrorMu.Unlock()
	l.programErrors[name] = l.CompileAndRun(name, f)
	if l.programErrors[name] != nil {
		glog.Infof("Compile errors for %s:\n%s", name, l.programErrors[name])
		if l.compileOnly {
			return l.programErrors[name]
		}
	}
	return nil
}

const loaderTemplate = `
<h2 id="loader">Program Loader</h2>
{{range $name, $errors := $.Errors}}
<p><b>{{$name}}</b></p>
{{if $errors}}
<pre>{{$errors}}</pre>
{{else}}
<p>No compile errors</p>
{{end}}
<p>Total load errors {{index $.Loaderrors $name}}; successes: {{index $.Loadsuccess $name}}</p>
{{end}}
`

// WriteStatusHTML writes the current state of the loader as HTML to the given writer w.
func (l *Loader) WriteStatusHTML(w io.Writer) error {
	t, err := template.New("loader").Parse(loaderTemplate)
	if err != nil {
		return err
	}
	l.programErrorMu.RLock()
	defer l.programErrorMu.RUnlock()
	data := struct {
		Errors      map[string]error
		Loaderrors  map[string]string
		Loadsuccess map[string]string
	}{
		l.programErrors,
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
	}
	return t.Execute(w, data)
}

// CompileAndRun compiles a program read from the input, starting execution if
// it succeeds.  If an existing virtual machine of the same name already
// exists, the previous virtual machine is terminated and the new loaded over
// it.  If the new program fails to compile, any existing virtual machine with
// the same name remains running.
func (l *Loader) CompileAndRun(name string, input io.Reader) error {
	o := &Options{
		CompileOnly:          l.compileOnly,
		EmitAst:              l.dumpAst,
		EmitAstTypes:         l.dumpAstTypes,
		SyslogUseCurrentYear: l.syslogUseCurrentYear,
		OverrideLocation:     l.overrideLocation,
	}
	v, errs := Compile(name, input, o)
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

	l.handles[name] = &vmHandle{make(chan *tailer.LogLine), make(chan struct{})}
	nameCode := nameToCode(name)
	glog.Infof("Program %s has goroutine marker 0x%x", name, nameCode)
	started := make(chan struct{})
	go v.Run(nameCode, l.handles[name].lines, l.handles[name].done, started)
	<-started
	glog.Infof("Started %s", name)

	return nil
}

func nameToCode(name string) uint32 {
	return uint32(uint32(name[0])<<24 | uint32(name[1])<<16 | uint32(name[2])<<8 | uint32(name[3]))
}

// Loader handles the lifecycle of programs and virtual machines, by watching
// the configured program source directory, compiling changes to programs, and
// managing the running virtual machines that receive input from the lines
// channel.
type Loader struct {
	w  watcher.Watcher // watches for program changes
	fs afero.Fs        // filesystem interface
	ms *metrics.Store  // pointer to store to pass to compiler

	handles  map[string]*vmHandle // map of program names to virtual machines
	handleMu sync.RWMutex         // guards accesses to handles

	programErrors  map[string]error // errors from the last compile attempt of the program
	programErrorMu sync.RWMutex     // guards access to programErrors

	watcherDone chan struct{} // Synchronise shutdown of the watcher and lines handlers.
	VMsDone     chan struct{} // Notify mtail when all running VMs are shutdown.

	compileOnly          bool           // Only compile programs and report errors, do not load VMs.
	dumpAst              bool           // print the AST after parse
	dumpAstTypes         bool           // print the AST after type check
	dumpBytecode         bool           // Instructs the loader to dump to stdout the compiled program after compilation.
	syslogUseCurrentYear bool           // Instructs the VM to overwrite zero years with the current year in a strptime instruction.
	overrideLocation     *time.Location // Instructs the vm to override the timezone with the specified zone.
	omitMetricSource     bool
}

// LoaderOptions contains the required and optional parameters for creating a
// new Loader.
type LoaderOptions struct {
	Store *metrics.Store
	Lines <-chan *tailer.LogLine

	W  watcher.Watcher // Not required, will use watcher.LogWatcher if zero.
	FS afero.Fs        // Not required, will use afero.OsFs if zero.

	CompileOnly          bool           // Compile, don't start execution.
	DumpAst              bool           // print the AST after type check
	DumpAstTypes         bool           // Instructs the loader to dump to stdout the compiled program after compilation.
	DumpBytecode         bool           // Instructs the loader to dump the program bytecode after compilation.
	SyslogUseCurrentYear bool           // If true, override empty year with the current in strptime().
	OverrideLocation     *time.Location // if not nil, overrides the timezone in strptime().
	OmitMetricSource     bool           // Don't put the source in the metric when added to the Store.
}

// NewLoader creates a new program loader.  It takes a filesystem watcher
// and a filesystem interface as arguments.  If fs is nil, it will use the
// default filesystem interface.
func NewLoader(o LoaderOptions) (*Loader, error) {
	if o.Store == nil || o.Lines == nil {
		return nil, errors.New("loader needs a store and lines")
	}
	fs := o.FS
	if fs == nil {
		fs = &afero.OsFs{}
	}
	w := o.W
	if w == nil {
		var err error
		w, err = watcher.NewLogWatcher()
		if err != nil {
			return nil, errors.Wrap(err, "Couldn't create a watcher for loader")
		}
	}

	l := &Loader{
		w:                    w,
		ms:                   o.Store,
		fs:                   fs,
		handles:              make(map[string]*vmHandle),
		programErrors:        make(map[string]error),
		watcherDone:          make(chan struct{}),
		VMsDone:              make(chan struct{}),
		compileOnly:          o.CompileOnly,
		dumpAst:              o.DumpAst,
		dumpAstTypes:         o.DumpAstTypes,
		dumpBytecode:         o.DumpBytecode,
		syslogUseCurrentYear: o.SyslogUseCurrentYear,
		overrideLocation:     o.OverrideLocation,
		omitMetricSource:     o.OmitMetricSource,
	}

	eventsChan := l.w.Events()
	go l.processEvents(eventsChan)
	go l.processLines(o.Lines)
	return l, nil
}

type vmHandle struct {
	lines chan *tailer.LogLine
	done  chan struct{}
}

// processEvents manages program lifecycle triggered by events from the
// filesystem watcher.
func (l *Loader) processEvents(events <-chan watcher.Event) {
	defer close(l.watcherDone)
	for event := range events {
		switch event := event.(type) {
		case watcher.DeleteEvent:
			l.UnloadProgram(event.Pathname)
		case watcher.UpdateEvent:
			l.LoadProg(event.Pathname)
		case watcher.CreateEvent:
			l.w.Add(event.Pathname)
		default:
			glog.V(1).Infof("Unexected event type %+#v", event)
		}
	}
}

// processLines provides fanout of the input log lines to each virtual machine
// running.  Upon close of the incoming lines channel, it also communicates
// shutdown to the target VMs via channel close.
func (l *Loader) processLines(lines <-chan *tailer.LogLine) {
	for logline := range lines {
		LineCount.Add(1)
		l.handleMu.RLock()
		for prog := range l.handles {
			l.handles[prog].lines <- logline
		}
		l.handleMu.RUnlock()
	}
	glog.Info("Shutting down loader.")
	l.w.Close()
	<-l.watcherDone
	l.handleMu.Lock()
	defer l.handleMu.Unlock()
	for prog := range l.handles {
		close(l.handles[prog].lines)
		<-l.handles[prog].done
		delete(l.handles, prog)
	}
	close(l.VMsDone)
}

// UnloadProgram removes the named program from the watcher to prevent future
// updates, and terminates any currently running VM goroutine.
func (l *Loader) UnloadProgram(pathname string) {
	if err := l.w.Remove(pathname); err != nil {
		glog.Infof("Remove watch on %s failed: %s", pathname, err)
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
