// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

// mtail programs may be updated while emtail is running, and they will be
// reloaded without having to restart the mtail process. Programs can be
// created and deleted as well, and some configuration systems do an atomic
// rename of the program when it is installed, so mtail is also aware of file
// moves.

import (
	"errors"
	"expvar"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"sync"

	"github.com/golang/glog"
	"github.com/spf13/afero"

	"github.com/google/mtail/metrics"
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
// directory for filesystem changes.  The total number of program errors is
// returned.
func (l *Loader) LoadProgs(programPath string) error {
	l.w.Add(programPath)

	s, err := os.Stat(programPath)
	if err != nil {
		return fmt.Errorf("failed to stat: %s", err)
	}
	switch {
	case s.IsDir():
		fis, err := ioutil.ReadDir(programPath)
		if err != nil {
			return fmt.Errorf("Failed to list programs in %q: %s", programPath, err)
		}

		for _, fi := range fis {
			if fi.IsDir() {
				continue
			}
			err = l.LoadProg(path.Join(programPath, fi.Name()))
			if err != nil {
				return err
			}
		}
		return nil
	default:
		return l.LoadProg(programPath)
	}
}

// LoadProg loads or reloads a program from the path specified.  The name of
// the program is the basename of the file.
func (l *Loader) LoadProg(programPath string) error {
	name := filepath.Base(programPath)
	if filepath.Ext(name) != fileExt {
		glog.Infof("Skipping %s due to file extension.", programPath)
		return nil
	}
	f, err := l.fs.Open(programPath)
	if err != nil {
		ProgLoadErrors.Add(name, 1)
		return fmt.Errorf("Failed to read program %q: %s", programPath, err)
	}
	defer f.Close()
	return l.CompileAndRun(name, f)
}

// CompileAndRun compiles a program read from the input, starting execution if
// it succeeds.  If an existing virtual machine of the same name already
// exists, the previous virtual machine is terminated and the new loaded over
// it.  If the new program fails to compile, any existing virtual machine with
// the same name remains running.
func (l *Loader) CompileAndRun(name string, input io.Reader) error {
	o := &Options{CompileOnly: l.compileOnly, SyslogUseCurrentYear: l.syslogUseCurrentYear}
	v, errs := Compile(name, input, o)
	if errs != nil {
		ProgLoadErrors.Add(name, 1)
		return fmt.Errorf("compile failed for %s:\n%s", name, errs)
	}
	if l.compileOnly {
		return nil
	}
	if v == nil {
		return fmt.Errorf("Internal error: Compilation failed for %s: No program returned, but no errors.", name)
	}
	for _, m := range v.m {
		if !m.Hidden {
			l.ms.Add(m)
		}
	}
	if l.dumpBytecode {
		v.DumpByteCode(name)
	}
	ProgLoads.Add(name, 1)
	glog.Infof("Loaded program %s", name)

	l.handleMu.Lock()
	defer l.handleMu.Unlock()
	// Stop any previous VM.
	if handle, ok := l.handles[name]; ok {
		close(handle.lines)
		<-handle.done
	}
	l.handles[name] = &vmHandle{make(chan string), make(chan struct{})}
	go v.Run(l.handles[name].lines, l.handles[name].done)
	return nil
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

	watcherDone chan struct{} // Synchronise shutdown of the watcher and lines handlers.
	VMsDone     chan struct{} // Notify mtail when all running VMs are shutdown.

	compileOnly          bool // Only compile programs and report errors, do not load VMs.
	dumpBytecode         bool // Instructs the loader to dump to stdout the compiled program after compilation.
	syslogUseCurrentYear bool // Instructs the VM to overwrite zero years with the current year in a strptime instruction.
}

// LoaderOptions contains the required and optional parameters for creating a
// new Loader.
type LoaderOptions struct {
	Store *metrics.Store
	Lines <-chan string
	W     watcher.Watcher // Not required, will use watcher.LogWatcher if zero.
	FS    afero.Fs        // Not required, will use afero.OsFs if zero.

	CompileOnly          bool
	DumpBytecode         bool
	SyslogUseCurrentYear bool
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
			return nil, fmt.Errorf("Couldn't create a watcher for loader: %s", err)
		}
	}
	l := &Loader{
		w:                    w,
		ms:                   o.Store,
		fs:                   fs,
		handles:              make(map[string]*vmHandle),
		watcherDone:          make(chan struct{}),
		VMsDone:              make(chan struct{}),
		compileOnly:          o.CompileOnly,
		dumpBytecode:         o.DumpBytecode,
		syslogUseCurrentYear: o.SyslogUseCurrentYear}

	go l.processEvents()
	go l.processLines(o.Lines)
	return l, nil
}

type vmHandle struct {
	lines chan string
	done  chan struct{}
}

// processEvents manages program lifecycle triggered by events from the
// filesystem watcher.
func (l *Loader) processEvents() {
	defer close(l.watcherDone)
	for event := range l.w.Events() {
		switch event := event.(type) {
		case watcher.DeleteEvent:
			glog.Infof("delete prog %s", event.Pathname)
			l.UnloadProgram(event.Pathname)
		case watcher.UpdateEvent:
			glog.Infof("update prog %s", event.Pathname)
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
func (l *Loader) processLines(lines <-chan string) {
	for line := range lines {
		LineCount.Add(1)
		l.handleMu.RLock()
		for prog := range l.handles {
			l.handles[prog].lines <- line
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
