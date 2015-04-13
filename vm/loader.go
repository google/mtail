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
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"path"
	"path/filepath"
	"strings"
	"sync"

	"github.com/golang/glog"
	"github.com/jaqx0r/afero"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/watcher"
)

var (
	// ProgLoads counts the number of program load events.
	ProgLoads = expvar.NewMap("prog_loads_total")
	// ProgLoadErrors counts the number of program load errors.
	ProgLoadErrors = expvar.NewMap("prog_load_errors")

	// DumpBytecode instructs the loader to dump the compiled program after compilation, for debugging.
	DumpBytecode = flag.Bool("dump_bytecode", false, "Dump bytecode of programs and exit.")
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
func (l *Loader) LoadProgs(programPath string) int {
	l.w.Add(programPath)

	fis, err := ioutil.ReadDir(programPath)
	if err != nil {
		glog.Fatalf("Failed to list programs in %q: %s", programPath, err)
	}

	errors := 0
	for _, fi := range fis {
		if fi.IsDir() {
			continue
		}
		errors += l.LoadProg(path.Join(programPath, fi.Name()))
	}
	return errors
}

// LoadProg loads or reloads a program from the path specified.  The name of
// the program is the basename of the file.
func (l *Loader) LoadProg(programPath string) (errors int) {
	name := filepath.Base(programPath)
	if filepath.Ext(name) != fileExt {
		glog.Infof("Skipping %s due to file extension.", programPath)
		return
	}
	f, err := l.fs.Open(programPath)
	if err != nil {
		glog.Infof("Failed to read program %q: %s", programPath, err)
		errors = 1
		ProgLoadErrors.Add(name, 1)
		return
	}
	defer f.Close()
	l.CompileAndRun(name, f)
	return
}

// CompileAndRun compiles a program read from the input, starting execution if
// it succeeds.  If an existing virtual machine of the same name already
// exists, the previous virtual machine is terminated and the new loaded over
// it.  If the new program fails to compile, any existing virtual machine with
// the same name remains running.
func (l *Loader) CompileAndRun(name string, input io.Reader) error {
	v, errs := Compile(name, input, l.ms)
	if errs != nil {
		ProgLoadErrors.Add(name, 1)
		return fmt.Errorf("compile failed for %s: %s", name, strings.Join(errs, "\n"))
	}
	if *DumpBytecode {
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
}

// NewLoader creates a new program loader.  It takes a filesystem watcher
// and a filesystem interface as arguments.  If fs is nil, it will use the
// default filesystem interface.
func NewLoader(w watcher.Watcher, ms *metrics.Store, lines <-chan string) *Loader {
	return newLoaderWithFs(w, ms, lines, afero.OsFs{})
}

// newLoaderWithFs creates a new program loader with a supplied filesystem
// implementation, for testing.
func newLoaderWithFs(w watcher.Watcher, ms *metrics.Store, lines <-chan string, fs afero.Fs) *Loader {
	l := &Loader{
		w:           w,
		ms:          ms,
		fs:          fs,
		handles:     make(map[string]*vmHandle),
		watcherDone: make(chan struct{})}

	go l.processEvents()
	go l.processLines(lines)
	return l
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
}

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
