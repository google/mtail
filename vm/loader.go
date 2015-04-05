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

	"github.com/golang/glog"
	"github.com/jaqx0r/afero"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/watcher"
)

var (
	Prog_loads       = expvar.NewMap("prog_loads_total")
	Prog_load_errors = expvar.NewMap("prog_load_errors")

	Dump_bytecode *bool = flag.Bool("dump_bytecode", false, "Dump bytecode of programs and exit.")
)

const (
	fileext = ".mtail"
)

// LoadProgs loads all programs in a directory and starts watching the
// directory for filesystem changes.
func (p *Loader) LoadProgs(program_path string) (*Engine, int) {
	p.w.Add(program_path)

	fis, err := ioutil.ReadDir(program_path)
	if err != nil {
		glog.Fatalf("Failed to list programs in %q: %s", program_path, err)
	}

	errors := 0
	for _, fi := range fis {
		if fi.IsDir() {
			continue
		}
		errors += p.LoadProg(path.Join(program_path, fi.Name()))
	}
	return &p.E, errors
}

// LoadProg loads or reloads a program from the path specified.  The name of
// the program is the basename of the file.
func (p *Loader) LoadProg(program_path string) (errors int) {
	name := filepath.Base(program_path)
	if filepath.Ext(name) != fileext {
		glog.Infof("Skipping %s due to file extension.", program_path)
		return
	}
	f, err := p.fs.Open(program_path)
	if err != nil {
		glog.Infof("Failed to read program %q: %s", program_path, err)
		errors = 1
		Prog_load_errors.Add(name, 1)
		return
	}
	defer f.Close()
	v, errs := Compile(name, f, p.ms)
	if errs != nil {
		errors = 1
		for _, e := range errs {
			glog.Info(e)
		}
		Prog_load_errors.Add(name, 1)
		return
	}
	if *Dump_bytecode {
		v.DumpByteCode(name)
	}
	p.E.AddVm(name, v)
	Prog_loads.Add(name, 1)
	glog.Infof("Loaded %s", name)
	return
}

// Loader handles the lifecycle of programs and virtual machines, by watching
// the configured program source directory, compiling changes to programs, and
// managing the running virtual machines that receive input from the lines
// channel.
type Loader struct {
	w  watcher.Watcher
	E  Engine
	ms *metrics.Store
	fs afero.Fs
}

// NewLoader creates a new program loader.  It takes a filesystem watcher
// and a filesystem interface as arguments.  If fs is nil, it will use the
// default filesystem interface.
func NewLoader(w watcher.Watcher, ms *metrics.Store) *Loader {
	return newLoaderWithFs(w, ms, afero.OsFs{})
}

// newLoaderWithFs creates a new program loader with a supplied filesystem implementation, for testing.
func newLoaderWithFs(w watcher.Watcher, ms *metrics.Store, fs afero.Fs) (p *Loader) {
	p = &Loader{w: w,
		E:  make(map[string]*VM),
		ms: ms,
		fs: fs}

	go p.run()
	return
}

func (p *Loader) run() {
	for event := range p.w.Events() {
		switch event := event.(type) {
		case watcher.DeleteEvent:
			glog.Infof("delete prog")
			_, f := filepath.Split(event.Pathname)
			p.E.RemoveVm(f)
			if err := p.w.Remove(event.Pathname); err != nil {
				glog.Info("Remove watch failed:", err)
			}
		case watcher.UpdateEvent:
			glog.Infof("update prog")
			p.LoadProg(event.Pathname)
		default:
			glog.Info("Unexected event type %+#v", event)
		}
	}
}
