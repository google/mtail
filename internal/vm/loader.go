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
	"context"
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
	"github.com/prometheus/client_golang/prometheus"
	"go.opencensus.io/trace"

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
func (l *Loader) LoadAllPrograms() error {
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

// LoadProgram loads or reloads a program from the full pathname programPath.  The name of
// the program is the basename of the file.
func (l *Loader) LoadProgram(programPath string) error {
	name := filepath.Base(programPath)
	if strings.HasPrefix(name, ".") {
		glog.V(2).Infof("Skipping %s because it is a hidden file.", programPath)
		return nil
	}
	if filepath.Ext(name) != fileExt {
		glog.V(2).Infof("Skipping %s due to file extension.", programPath)
		return nil
	}
	f, err := os.OpenFile(programPath, os.O_RDONLY, 0600)
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
func (l *Loader) WriteStatusHTML(w io.Writer) error {
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
func (l *Loader) CompileAndRun(name string, input io.Reader) error {
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

	l.handles[name] = v
	return nil
}

// Loader handles the lifecycle of programs and virtual machines, by watching
// the configured program source directory, compiling changes to programs, and
// managing the running virtual machines that receive input from the lines
// channel.
type Loader struct {
	ms          *metrics.Store        // pointer to metrics.Store to pass to compiler
	w           watcher.Watcher       // watches for program changes
	reg         prometheus.Registerer // plce to reg metrics
	programPath string                // Path that contains mtail programs.

	eventsHandle int // record the handle with which to add programs to the watcher

	handleMu sync.RWMutex   // guards accesses to handles
	handles  map[string]*VM // map of program names to virtual machines

	programErrorMu sync.RWMutex     // guards access to programErrors
	programErrors  map[string]error // errors from the last compile attempt of the program

	watcherDone chan struct{} // Synchronise shutdown of the watcher processEvents goroutine

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
func OverrideLocation(loc *time.Location) func(*Loader) error {
	return func(l *Loader) error {
		l.overrideLocation = loc
		return nil
	}
}

// CompileOnly sets the Loader to compile programs only, without executing them.
func CompileOnly(l *Loader) error {
	l.compileOnly = true
	return ErrorsAbort(l)
}

// ErrorsAbort sets the Loader to abort the Loader on compile errors.
func ErrorsAbort(l *Loader) error {
	l.errorsAbort = true
	return nil
}

// DumpAst instructs the Loader to print the AST after program compilation.
func DumpAst(l *Loader) error {
	l.dumpAst = true
	return nil
}

// DumpAstTypes instructs the Loader to print the AST after type checking.
func DumpAstTypes(l *Loader) error {
	l.dumpAstTypes = true
	return nil
}

// DumpBytecode instructs the loader to print the compiled bytecode after code generation.
func DumpBytecode(l *Loader) error {
	l.dumpBytecode = true
	return nil
}

// SyslogUseCurrentYear instructs the VM to annotate yearless timestamps with the current year.
func SyslogUseCurrentYear(l *Loader) error {
	l.syslogUseCurrentYear = true
	return nil
}

// OmitMetricSource instructs the Loader to not annotate metrics with their program source when added to the metric store.
func OmitMetricSource(l *Loader) error {
	l.omitMetricSource = true
	return nil
}

// PrometheusRegisterer passes in a registry for setting up exported metrics.
func PrometheusRegisterer(reg prometheus.Registerer) func(l *Loader) error {
	return func(l *Loader) error {
		l.reg = reg
		return nil
	}
}

// NewLoader creates a new program loader that reads programs from programPath.
func NewLoader(programPath string, store *metrics.Store, lines <-chan *logline.LogLine, w watcher.Watcher, options ...func(*Loader) error) (*Loader, error) {
	if store == nil || lines == nil {
		return nil, errors.New("loader needs a store and lines")
	}
	l := &Loader{
		ms:            store,
		w:             w,
		programPath:   programPath,
		handles:       make(map[string]*VM),
		programErrors: make(map[string]error),
		watcherDone:   make(chan struct{}),
	}
	if err := l.SetOption(options...); err != nil {
		return nil, err
	}
	if l.reg != nil {
		l.reg.MustRegister(lineProcessingDurations)
	}
	handle, eventsChan := l.w.Events()
	l.eventsHandle = handle
	go l.processEvents(eventsChan)
	go l.processLines(lines)
	return l, nil
}

// SetOption takes one or more option functions and applies them in order to Loader.
func (l *Loader) SetOption(options ...func(*Loader) error) error {
	for _, option := range options {
		if err := option(l); err != nil {
			return err
		}
	}
	return nil
}

// processEvents manages program lifecycle triggered by events from the
// filesystem watcher.
func (l *Loader) processEvents(events <-chan watcher.Event) {
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
// shutdown to the target VMs via channel close.
func (l *Loader) processLines(lines <-chan *logline.LogLine) {
	// Copy all input LogLines to each VM's LogLine input channel.
	for ll := range lines {
		ctx, span := trace.StartSpan(ll.Context, "loader.processLines")
		span.AddMessageReceiveEvent(1, int64(len(ll.Line)), int64(len(ll.Line)))
		l.ProcessLogLine(ctx, ll)
		span.End()
	}
	l.Close()
}

func (l *Loader) Close() {
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
		delete(l.handles, prog)
	}
}

// ProcessLogLine satisfies the LogLine.Processor interface.
func (l *Loader) ProcessLogLine(ctx context.Context, ll *logline.LogLine) {
	ctx, span := trace.StartSpan(ctx, "Loader.ProcessLogLine")
	defer span.End()
	LineCount.Add(1)
	l.handleMu.RLock()
	defer l.handleMu.RUnlock()
	for prog := range l.handles {
		l.handles[prog].ProcessLogLine(ctx, ll)
	}
}

// UnloadProgram removes the named program from the watcher to prevent future
// updates, and terminates any currently running VM goroutine.
func (l *Loader) UnloadProgram(pathname string) {
	if err := l.w.Remove(pathname); err != nil {
		glog.V(2).Infof("Remove watch on %s failed: %s", pathname, err)
	}
	name := filepath.Base(pathname)
	l.handleMu.Lock()
	defer l.handleMu.Unlock()
	if _, ok := l.handles[name]; ok {
		delete(l.handles, name)
	}
}
