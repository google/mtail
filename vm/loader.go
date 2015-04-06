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
	"io/ioutil"
	"path"
	"path/filepath"
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
func (p *Loader) LoadProgs(programPath string) int {
	p.w.Add(programPath)

	fis, err := ioutil.ReadDir(programPath)
	if err != nil {
		glog.Fatalf("Failed to list programs in %q: %s", programPath, err)
	}

	errors := 0
	for _, fi := range fis {
		if fi.IsDir() {
			continue
		}
		errors += p.LoadProg(path.Join(programPath, fi.Name()))
	}
	return errors
}

// LoadProg loads or reloads a program from the path specified.  The name of
// the program is the basename of the file.
func (p *Loader) LoadProg(programPath string) (errors int) {
	name := filepath.Base(programPath)
	if filepath.Ext(name) != fileExt {
		glog.Infof("Skipping %s due to file extension.", programPath)
		return
	}
	f, err := p.fs.Open(programPath)
	if err != nil {
		glog.Infof("Failed to read program %q: %s", programPath, err)
		errors = 1
		ProgLoadErrors.Add(name, 1)
		return
	}
	defer f.Close()
	v, errs := Compile(name, f, p.ms)
	if errs != nil {
		errors = 1
		for _, e := range errs {
			glog.Info(e)
		}
		ProgLoadErrors.Add(name, 1)
		return
	}
	if *DumpBytecode {
		v.DumpByteCode(name)
	}

	ProgLoads.Add(name, 1)
	glog.Infof("Loaded %s", name)

	return
}

// Loader handles the lifecycle of programs and virtual machines, by watching
// the configured program source directory, compiling changes to programs, and
// managing the running virtual machines that receive input from the lines
// channel.
type Loader struct {
	w  watcher.Watcher // watches for program changes
	fs afero.Fs        // filesystem interface
	ms *metrics.Store  // pointer to store to pass to compiler

	linesLock sync.Mutex               // guards accesses to lines
	lines     map[string]chan<- string // map of program names to input channels
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
	l := &Loader{w: w,
		ms: ms,
		fs: fs}

	go l.handleEvents()
	go l.handleLines(lines)
	return l
}

// handleEvents manages program lifecycle triggered by events from the
// filesystem watcher.
func (p *Loader) handleEvents() {
	for event := range p.w.Events() {
		switch event := event.(type) {
		case watcher.DeleteEvent:
			glog.Infof("delete prog")
			f := filepath.Base(event.Pathname)
			p.linesLock.Lock()
			close(p.lines[f])
			delete(p.lines, f)
			if err := p.w.Remove(event.Pathname); err != nil {
				glog.Info("Remove watch failed:", err)
			}
		case watcher.UpdateEvent:
			glog.Infof("update prog")
			p.LoadProg(event.Pathname)
		case watcher.CreateEvent:
			p.LoadProg(event.Pathname)
		default:
			glog.Info("Unexected event type %+#v", event)
		}
	}
}

// handleLines provides fanout of the input log lines to each virtual machine
// running.  Upon close of the incoming lines channel, it also communicates
// shutdown to the target VMs via channel close.
func (l *Loader) handleLines(lines <-chan string) {
	for line := range lines {
		LineCount.Add(1)
		l.linesLock.Lock()
		for prog := range l.lines {
			l.lines[prog] <- line
		}
		l.linesLock.Unlock()
	}
	glog.Info("Shutting down loader.")
	l.linesLock.Lock()
	for prog := range l.lines {
		close(l.lines[prog])
	}
	l.linesLock.Unlock()
}
