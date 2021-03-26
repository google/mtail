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
	"bytes"
	"context"
	"crypto/sha256"
	"expvar"
	"fmt"
	"html/template"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
)

var (
	// LineCount counts the number of lines received by the program loader.
	LineCount = expvar.NewInt("lines_total")
	// ProgLoads counts the number of program load events.
	ProgLoads = expvar.NewMap("prog_loads_total")
	// ProgLoadErrors counts the number of program load errors.
	ProgLoadErrors    = expvar.NewMap("prog_load_errors_total")
	progRuntimeErrors = expvar.NewMap("prog_runtime_errors_total")
)

const (
	fileExt = ".mtail"
)

// LoadAllPrograms loads all programs in a directory and starts watching the
// directory for filesystem changes.  Any compile errors are stored for later retrieival.
// This function returns an error if an internal error occurs.
func (l *Loader) LoadAllPrograms() error {
	if l.programPath == "" {
		glog.V(2).Info("Programpath is empty, loading nothing")
		return nil
	}
	s, err := os.Stat(l.programPath)
	if err != nil {
		return errors.Wrapf(err, "failed to stat %q", l.programPath)
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
			err = l.LoadProgram(filepath.Join(l.programPath, fi.Name()))
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
<th>last runtime error</th>
</tr>
<tr>
{{range $name, $errors := $.Errors}}
<td><a href="/progz?prog={{$name}}">{{$name}}</a></td>
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
<td><pre>{{index $.RuntimeErrorString $name}}</pre></td>
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
		Errors             map[string]error
		Loaderrors         map[string]string
		Loadsuccess        map[string]string
		RuntimeErrors      map[string]string
		RuntimeErrorString map[string]string
	}{
		l.programErrors,
		make(map[string]string),
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
		data.RuntimeErrorString[name] = l.handles[name].vm.RuntimeErrorString()
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
	var buf bytes.Buffer
	tee := io.TeeReader(input, &buf)
	hasher := sha256.New()
	if _, err := io.Copy(hasher, tee); err != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Wrapf(err, "hashing failed for %q", name)
	}
	contentHash := hasher.Sum(nil)
	l.handleMu.RLock()
	vm, ok := l.handles[name]
	l.handleMu.RUnlock()
	if ok && bytes.Equal(vm.contentHash, contentHash) {
		glog.V(1).Infof("contents match, not recompiling %q", name)
		return nil
	}
	v, errs := Compile(name, &buf, l.dumpAst, l.dumpAstTypes, l.syslogUseCurrentYear, l.overrideLocation, l.maxRegexpLength, l.maxRecursionDepth)
	if errs != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Errorf("compile failed for %s:\n%s", name, errs)
	}
	if v == nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Errorf("Internal error: Compilation failed for %s: No program returned, but no errors.", name)
	}

	if l.dumpBytecode {
		glog.Info("Dumping program objects and bytecode\n", v.DumpByteCode())
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
	// Terminates the existing vm.
	if handle, ok := l.handles[name]; ok {
		close(handle.lines)
	}
	lines := make(chan *logline.LogLine)
	l.handles[name] = &vmHandle{contentHash: contentHash, vm: v, lines: lines}
	l.wg.Add(1)
	go v.Run(lines, &l.wg)
	return nil
}

type vmHandle struct {
	contentHash []byte
	vm          *VM
	lines       chan *logline.LogLine
}

// Loader handles the lifecycle of programs and virtual machines, by watching
// the configured program source directory, compiling changes to programs, and
// managing the virtual machines.
type Loader struct {
	ctx         context.Context       // a cancellable context
	wg          sync.WaitGroup        // used to await vm shutdown
	ms          *metrics.Store        // pointer to metrics.Store to pass to compiler
	reg         prometheus.Registerer // plce to reg metrics
	programPath string                // Path that contains mtail programs.

	handleMu sync.RWMutex         // guards accesses to handles
	handles  map[string]*vmHandle // map of program names to virtual machines

	programErrorMu sync.RWMutex     // guards access to programErrors
	programErrors  map[string]error // errors from the last compile attempt of the program

	overrideLocation     *time.Location // Instructs the vm to override the timezone with the specified zone.
	compileOnly          bool           // Only compile programs and report errors, do not load VMs.
	errorsAbort          bool           // Compiler errors abort the loader.
	dumpAst              bool           // print the AST after parse
	dumpAstTypes         bool           // print the AST after type check
	dumpBytecode         bool           // Instructs the loader to dump to stdout the compiled program after compilation.
	syslogUseCurrentYear bool           // Instructs the VM to overwrite zero years with the current year in a strptime instruction.
	omitMetricSource     bool

	signalQuit        chan struct{} // When closed stops the signal handler goroutine.
	maxRegexpLength   int
	maxRecursionDepth int
}

// Option configures a new program Loader.
type Option func(*Loader) error

// OverrideLocation sets the timezone location for the VM.
func OverrideLocation(loc *time.Location) Option {
	return func(l *Loader) error {
		l.overrideLocation = loc
		return nil
	}
}

// CompileOnly sets the Loader to compile programs only, without executing them.
func CompileOnly() Option {
	return func(l *Loader) error {
		l.compileOnly = true
		return ErrorsAbort()(l)
	}
}

// ErrorsAbort sets the Loader to abort the Loader on compile errors.
func ErrorsAbort() Option {
	return func(l *Loader) error {
		l.errorsAbort = true
		return nil
	}
}

// DumpAst instructs the Loader to print the AST after program compilation.
func DumpAst() Option {
	return func(l *Loader) error {
		l.dumpAst = true
		return nil
	}
}

// DumpAstTypes instructs the Loader to print the AST after type checking.
func DumpAstTypes() Option {
	return func(l *Loader) error {
		l.dumpAstTypes = true
		return nil
	}
}

// DumpBytecode instructs the loader to print the compiled bytecode after code generation.
func DumpBytecode() Option {
	return func(l *Loader) error {
		l.dumpBytecode = true
		return nil
	}
}

// SyslogUseCurrentYear instructs the VM to annotate yearless timestamps with the current year.
func SyslogUseCurrentYear() Option {
	return func(l *Loader) error {
		l.syslogUseCurrentYear = true
		return nil
	}
}

// MaxRegexpLength sets the maximum length an mtail regular expression can have, in terms of characters.
func MaxRegexpLength(maxRegexpLength int) Option {
	return func(l *Loader) error {
		l.maxRegexpLength = maxRegexpLength
		return nil
	}
}

// MaxRecursionDepth sets the maximum depth the abstract syntax tree built during lexation can have
func MaxRecursionDepth(maxRecursionLength int) Option {
	return func(l *Loader) error {
		l.maxRecursionDepth = maxRecursionLength
		return nil
	}
}

// OmitMetricSource instructs the Loader to not annotate metrics with their program source when added to the metric store.
func OmitMetricSource() Option {
	return func(l *Loader) error {
		l.omitMetricSource = true
		return nil
	}
}

// PrometheusRegisterer passes in a registry for setting up exported metrics.
func PrometheusRegisterer(reg prometheus.Registerer) Option {
	return func(l *Loader) error {
		l.reg = reg
		l.reg.MustRegister(lineProcessingDurations)
		return nil
	}
}

// NewLoader creates a new program loader that reads programs from programPath.
func NewLoader(lines <-chan *logline.LogLine, wg *sync.WaitGroup, programPath string, store *metrics.Store, options ...Option) (*Loader, error) {
	if store == nil {
		return nil, errors.New("loader needs a store")
	}
	l := &Loader{
		ms:            store,
		programPath:   programPath,
		handles:       make(map[string]*vmHandle),
		programErrors: make(map[string]error),
		signalQuit:    make(chan struct{}),
	}
	initDone := make(chan struct{})
	defer close(initDone)
	if err := l.SetOption(options...); err != nil {
		return nil, err
	}
	// Defer shutdown handling to avoid a race on l.wg.
	wg.Add(1)
	defer func() {
		go func() {
			defer wg.Done()
			<-initDone
			l.wg.Wait()
		}()
	}()
	// This goroutine is the main consumer/producer loop.
	l.wg.Add(1)
	go func() {
		defer l.wg.Done() // signal to owner we're done
		<-initDone
		for line := range lines {
			LineCount.Add(1)
			l.handleMu.RLock()
			for prog := range l.handles {
				l.handles[prog].lines <- line
			}
			l.handleMu.RUnlock()
		}
		glog.Info("END OF LINE")
		close(l.signalQuit)
		l.handleMu.Lock()
		for prog := range l.handles {
			close(l.handles[prog].lines)
			delete(l.handles, prog)
		}
		l.handleMu.Unlock()
	}()
	if l.programPath == "" {
		glog.Info("No program path specified, no programs will be loaded.")
		return l, nil
	}

	// Create one goroutine that handles reload signals.
	l.wg.Add(1)
	go func() {
		defer l.wg.Done()
		<-initDone
		if l.programPath == "" {
			glog.Info("no program reload on SIGHUP without programPath")
			return
		}
		n := make(chan os.Signal, 1)
		signal.Notify(n, syscall.SIGHUP)
		defer signal.Stop(n)
		for {
			select {
			case <-l.signalQuit:
				return
			case <-n:
				if err := l.LoadAllPrograms(); err != nil {
					glog.Info(err)
				}
			}
		}
	}()
	// Guarantee all existing programmes get loaded before we leave.
	if err := l.LoadAllPrograms(); err != nil {
		return nil, err
	}
	return l, nil
}

// SetOption takes one or more option functions and applies them in order to Loader.
func (l *Loader) SetOption(options ...Option) error {
	for _, option := range options {
		if err := option(l); err != nil {
			return err
		}
	}
	return nil
}

// UnloadProgram removes the named program, any currently running VM goroutine.
func (l *Loader) UnloadProgram(pathname string) {
	name := filepath.Base(pathname)
	l.handleMu.Lock()
	defer l.handleMu.Unlock()
	if _, ok := l.handles[name]; ok {
		delete(l.handles, name)
	}
}

func (l *Loader) ProgzHandler(w http.ResponseWriter, r *http.Request) {
	prog := r.URL.Query().Get("prog")
	if prog != "" {
		l.handleMu.RLock()
		handle, ok := l.handles[prog]
		l.handleMu.RUnlock()
		if !ok {
			http.Error(w, "No program found", http.StatusNotFound)
			return
		}
		fmt.Fprintf(w, handle.vm.DumpByteCode())
		fmt.Fprintf(w, "\nLast runtime error:\n%s", handle.vm.RuntimeErrorString())
		return
	}
	l.handleMu.RLock()
	defer l.handleMu.RUnlock()
	w.Header().Add("Content-type", "text/html")
	fmt.Fprintf(w, "<ul>")
	for prog := range l.handles {
		fmt.Fprintf(w, "<li><a href=\"?prog=%s\">%s</a></li>", prog, prog)
	}
	fmt.Fprintf(w, "</ul>")
}
