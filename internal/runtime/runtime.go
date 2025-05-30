// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

// mtail programs may be created, updated, and deleted while mtail is running, and they will be
// reloaded without having to restart the mtail process -- mtail will handle these on a HUP signal.

import (
	"bytes"
	"crypto/sha256"
	"encoding/json"
	"expvar"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/runtime/compiler"
	"github.com/jaqx0r/mtail/internal/runtime/vm"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
	"gopkg.in/yaml.v3"
)

var (
	// LineCount counts the number of lines received by the program loader.
	LineCount = expvar.NewInt("lines_total")
	// ProgLoads counts the number of program load events.
	ProgLoads = expvar.NewMap("prog_loads_total")
	// ProgUnloads counts the number of program unload events.
	ProgUnloads = expvar.NewMap("prog_unloads_total")
	// ProgLoadErrors counts the number of program load errors.
	ProgLoadErrors = expvar.NewMap("prog_load_errors_total")
)

const (
	fileExt = ".mtail"
)

// LoadAllPrograms loads all programs in a directory and starts watching the
// directory for filesystem changes.  Any compile errors are stored for later retrieival.
// This function returns an error if an internal error occurs.
func (r *Runtime) LoadAllPrograms() error {
	if r.programPath == "" {
		glog.V(2).Info("Programpath is empty, loading nothing")
		return nil
	}
	s, err := os.Stat(r.programPath)
	if err != nil {
		return errors.Wrapf(err, "failed to stat %q", r.programPath)
	}
	switch {
	case s.IsDir():
		dirents, rerr := os.ReadDir(r.programPath)
		if rerr != nil {
			return errors.Wrapf(rerr, "Failed to list programs in %q", r.programPath)
		}

		markDeleted := make(map[string]struct{})
		r.handleMu.RLock()
		for name := range r.handles {
			glog.Infof("added %s", name)
			markDeleted[name] = struct{}{}
		}
		r.handleMu.RUnlock()
		for _, dirent := range dirents {
			if dirent.IsDir() {
				continue
			}
			err = r.LoadProgram(filepath.Join(r.programPath, dirent.Name()))
			if err != nil {
				if r.errorsAbort {
					return err
				}
				glog.Warning(err)
			}
			glog.Infof("unmarking %s", filepath.Base(dirent.Name()))
			delete(markDeleted, filepath.Base(dirent.Name()))
		}
		for name := range markDeleted {
			glog.Infof("unloading %s", name)
			r.UnloadProgram(name)
		}
	default:
		err = r.LoadProgram(r.programPath)
		if err != nil {
			if r.errorsAbort {
				return err
			}
			glog.Warning(err)
		}
	}
	return nil
}

// LoadProgram loads or reloads a program from the full pathname programPath.  The name of
// the program is the basename of the file.
func (r *Runtime) LoadProgram(programPath string) error {
	name := filepath.Base(programPath)
	if strings.HasPrefix(name, ".") {
		glog.V(2).Infof("Skipping %s because it is a hidden file.", programPath)
		return nil
	}
	if filepath.Ext(name) != fileExt {
		glog.V(2).Infof("Skipping %s due to file extension.", programPath)
		return nil
	}
	f, err := os.OpenFile(filepath.Clean(programPath), os.O_RDONLY, 0o600)
	if err != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Wrapf(err, "Failed to read program %q", programPath)
	}
	defer func() {
		if err := f.Close(); err != nil {
			glog.Warning(err)
		}
	}()
	r.programErrorMu.Lock()
	defer r.programErrorMu.Unlock()
	r.programErrors[name] = r.CompileAndRun(name, f)
	if r.programErrors[name] != nil {
		if r.errorsAbort {
			return r.programErrors[name]
		}
		glog.Infof("Compile errors for %s:\n%s", name, r.programErrors[name])
	}
	return nil
}

// CompileAndRun compiles a program read from the input, starting execution if
// it succeeds.  If an existing virtual machine of the same name already
// exists, the previous virtual machine is terminated and the new loaded over
// it.  If the new program fails to compile, any existing virtual machine with
// the same name remains running.
func (r *Runtime) CompileAndRun(name string, input io.Reader) error {
	glog.V(2).Infof("CompileAndRun %s", name)
	var buf bytes.Buffer
	tee := io.TeeReader(input, &buf)
	hasher := sha256.New()
	if _, err := io.Copy(hasher, tee); err != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Wrapf(err, "hashing failed for %q", name)
	}
	contentHash := hasher.Sum(nil)
	r.handleMu.RLock()
	vh, ok := r.handles[name]
	r.handleMu.RUnlock()
	if ok && bytes.Equal(vh.contentHash, contentHash) {
		glog.V(1).Infof("contents match, not recompiling %q", name)
		return nil
	}
	obj, errs := r.c.Compile(name, &buf)
	if errs != nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Errorf("compile failed for %s:\n%s", name, errs)
	}
	if obj == nil {
		ProgLoadErrors.Add(name, 1)
		return errors.Errorf("internal error: compilation failed for %s: no program returned, but no errors", name)
	}
	v := vm.New(name, obj, r.syslogUseCurrentYear, r.overrideLocation, r.logRuntimeErrors, r.trace)

	if r.dumpBytecode {
		glog.Info("Dumping program objects and bytecode\n", v.DumpByteCode())
	}

	// Load the metrics from the compilation into the global metric storage for export.
	for _, m := range v.Metrics {
		if !m.Hidden {
			if r.omitMetricSource {
				m.Source = ""
			}
			err := r.ms.Add(m)
			if err != nil {
				return err
			}
		}
	}

	ProgLoads.Add(name, 1)
	glog.Infof("Loaded program %s", name)

	if r.compileOnly {
		return nil
	}

	r.handleMu.Lock()
	defer r.handleMu.Unlock()
	// Terminates the existing vm.
	if handle, ok := r.handles[name]; ok {
		close(handle.lines)
	}
	lines := make(chan *logline.LogLine)
	r.handles[name] = &vmHandle{contentHash: contentHash, vm: v, lines: lines}
	r.wg.Add(1)
	go v.Run(lines, &r.wg)
	return nil
}

type vmHandle struct {
	contentHash []byte
	vm          *vm.VM
	lines       chan *logline.LogLine
}

// Runtime handles the lifecycle of programs and virtual machines, by watching
// the configured program source directory, compiling changes to programs, and
// managing the virtual machines.
type Runtime struct {
	wg sync.WaitGroup // used to await vm shutdown

	ms  *metrics.Store        // pointer to metrics.Store to pass to compiler
	reg prometheus.Registerer // plce to reg metrics

	cOpts []compiler.Option // options for constructing `c`
	c     *compiler.Compiler

	programPath string // Path that contains mtail programs.

	handleMu sync.RWMutex         // guards accesses to handles
	handles  map[string]*vmHandle // map of program names to virtual machines

	programErrorMu sync.RWMutex     // guards access to programErrors
	programErrors  map[string]error // errors from the last compile attempt of the program

	// Source to program mapping
	sourceToProgramsMu sync.RWMutex         // guards access to sourceToPrograms
	sourceToPrograms   map[string][]string  // Maps log sources to programs that should process them
	unmappedBehavior   string               // "all" (default) or "none" - what to do with unmapped sources

	overrideLocation     *time.Location // Instructs the vm to override the timezone with the specified zone.
	compileOnly          bool           // Only compile programs and report errors, do not load VMs.
	errorsAbort          bool           // Compiler errors abort the loader.
	dumpBytecode         bool           // Instructs the loader to dump to stdout the compiled program after compilation.
	syslogUseCurrentYear bool           // Instructs the VM to overwrite zero years with the current year in a strptime instruction.
	omitMetricSource     bool
	logRuntimeErrors     bool // Instruct the VM to emit runtime errors to the log.
	trace                bool // Trace execution of each VM.

	signalQuit chan struct{} // When closed stops the signal handler goroutine.
}

var (
	ErrNeedsStore     = errors.New("loader needs a store")
	ErrNeedsWaitgroup = errors.New("loader needs a WaitGroup")
)

// New creates a new program loader that reads programs from programPath.
func New(lines <-chan *logline.LogLine, wg *sync.WaitGroup, programPath string, store *metrics.Store, options ...Option) (*Runtime, error) {
	if store == nil {
		return nil, ErrNeedsStore
	}
	if wg == nil {
		return nil, ErrNeedsWaitgroup
	}
	r := &Runtime{
		ms:              store,
		programPath:     programPath,
		handles:         make(map[string]*vmHandle),
		programErrors:   make(map[string]error),
		sourceToPrograms: make(map[string][]string),
		unmappedBehavior: "all", // Default: process unmapped sources with all programs
		signalQuit:      make(chan struct{}),
	}
	initDone := make(chan struct{})
	defer close(initDone)
	var err error
	if err = r.SetOption(options...); err != nil {
		return nil, err
	}
	if r.c, err = compiler.New(r.cOpts...); err != nil {
		return nil, err
	}
	// Defer shutdown handling to avoid a race on r.wg.
	wg.Add(1)
	defer func() {
		go func() {
			defer wg.Done()
			<-initDone
			r.wg.Wait()
		}()
	}()
	// This goroutine is the main consumer/producer loop.
	r.wg.Add(1)
	go func() {
		defer r.wg.Done() // signal to owner we're done
		<-initDone
		for line := range lines {
			LineCount.Add(1)
			
			// Check if we have mapping for this source
			r.handleMu.RLock()
			r.sourceToProgramsMu.RLock()
			
			// Get the log filename
			filename := line.Filename
			
			if programs, ok := r.sourceToPrograms[filename]; ok {
				// Send only to mapped programs
				for _, progName := range programs {
					if handle, ok := r.handles[progName]; ok {
						handle.lines <- line
					}
				}
			} else if r.unmappedBehavior == "all" {
				// Default behavior (all programs)
				for prog := range r.handles {
					r.handles[prog].lines <- line
				}
			}
			// If unmappedBehavior is "none", we don't send the line to any program
			
			r.sourceToProgramsMu.RUnlock()
			r.handleMu.RUnlock()
		}
		glog.Info("END OF LINE")
		glog.Infof("processed %s lines", LineCount.String())
		close(r.signalQuit)
		r.handleMu.Lock()
		for prog := range r.handles {
			close(r.handles[prog].lines)
			delete(r.handles, prog)
		}
		r.handleMu.Unlock()
	}()
	if r.programPath == "" {
		glog.Info("No program path specified, no programs will be loaded.")
		return r, nil
	}

	// Create one goroutine that handles reload signals.
	r.wg.Add(1)
	go func() {
		defer r.wg.Done()
		<-initDone
		if r.programPath == "" {
			glog.Info("no program reload on SIGHUP without programPath")
			return
		}
		n := make(chan os.Signal, 1)
		signal.Notify(n, syscall.SIGHUP)
		defer signal.Stop(n)
		for {
			select {
			case <-r.signalQuit:
				return
			case <-n:
				if err := r.LoadAllPrograms(); err != nil {
					glog.Info(err)
				}
			}
		}
	}()
	// Guarantee all existing programmes get loaded before we leave.
	if err := r.LoadAllPrograms(); err != nil {
		return nil, err
	}
	return r, nil
}

// SetOption takes one or more option functions and applies them in order to Runtime.
func (r *Runtime) SetOption(options ...Option) error {
	for _, option := range options {
		if err := option(r); err != nil {
			return err
		}
	}
	return nil
}

// UnloadProgram removes the named program, any currently running VM goroutine.
func (r *Runtime) UnloadProgram(pathname string) {
	name := filepath.Base(pathname)
	r.handleMu.Lock()
	defer r.handleMu.Unlock()
	close(r.handles[name].lines)
	delete(r.handles, name)
	ProgUnloads.Add(name, 1)
}

// AddSourceMapping adds a mapping from a log source to a list of programs.
// If the source already has a mapping, it will be replaced.
func (r *Runtime) AddSourceMapping(source string, programs []string) {
	r.sourceToProgramsMu.Lock()
	defer r.sourceToProgramsMu.Unlock()
	r.sourceToPrograms[source] = programs
	glog.Infof("Added source mapping: %s -> %v", source, programs)
}

// RemoveSourceMapping removes a mapping for a log source.
func (r *Runtime) RemoveSourceMapping(source string) {
	r.sourceToProgramsMu.Lock()
	defer r.sourceToProgramsMu.Unlock()
	delete(r.sourceToPrograms, source)
	glog.Infof("Removed source mapping for: %s", source)
}

// GetSourceMappings returns a copy of the current source-to-program mappings.
func (r *Runtime) GetSourceMappings() map[string][]string {
	r.sourceToProgramsMu.RLock()
	defer r.sourceToProgramsMu.RUnlock()
	
	mappings := make(map[string][]string)
	for source, programs := range r.sourceToPrograms {
		progsCopy := make([]string, len(programs))
		copy(progsCopy, programs)
		mappings[source] = progsCopy
	}
	return mappings
}

// LoadSourceMappingsFromFile loads source-to-program mappings from a YAML or JSON file.
func (r *Runtime) LoadSourceMappingsFromFile(filename string) error {
	f, err := os.Open(filename)
	if err != nil {
		return errors.Wrapf(err, "failed to open source mapping file %q", filename)
	}
	defer f.Close()
	
	var config SourceMappingConfig
	
	// Determine the file format based on extension
	ext := filepath.Ext(filename)
	if ext == ".yaml" || ext == ".yml" {
		decoder := yaml.NewDecoder(f)
		if err := decoder.Decode(&config); err != nil {
			return errors.Wrapf(err, "failed to decode YAML in %q", filename)
		}
	} else if ext == ".json" {
		decoder := json.NewDecoder(f)
		if err := decoder.Decode(&config); err != nil {
			return errors.Wrapf(err, "failed to decode JSON in %q", filename)
		}
	} else {
		return errors.Errorf("unsupported file extension %q for source mapping file", ext)
	}
	
	// Apply unmapped behavior setting if specified
	if config.UnmappedBehavior != "" {
		if config.UnmappedBehavior != "all" && config.UnmappedBehavior != "none" {
			return errors.Errorf("invalid unmapped_behavior value: %s (must be 'all' or 'none')", config.UnmappedBehavior)
		}
		r.unmappedBehavior = config.UnmappedBehavior
	}
	
	// Apply mappings
	r.sourceToProgramsMu.Lock()
	defer r.sourceToProgramsMu.Unlock()
	
	// Clear existing mappings if we're loading a new configuration
	r.sourceToPrograms = make(map[string][]string)
	
	for _, mapping := range config.Mappings {
		if mapping.Source == "" {
			glog.Warning("Skipping mapping with empty source")
			continue
		}
		if len(mapping.Programs) == 0 {
			glog.Warningf("Skipping mapping for source %q with no programs", mapping.Source)
			continue
		}
		r.sourceToPrograms[mapping.Source] = mapping.Programs
		glog.Infof("Added source mapping: %s -> %v", mapping.Source, mapping.Programs)
	}
	
	return nil
}
