package vm

// mtail programs may be updated while emtail is running, and they will be
// reloaded without having to restart the mtail process. Programs can be
// created and deleted as well, and some configuration systems do an atomic
// rename of the program when it is installed, so mtail is also aware of file
// moves.

// prog lifecycle
// create/write/close
//

import (
	"expvar"
	"flag"
	"io/ioutil"
	"path"
	"path/filepath"
	"sync"

	"github.com/golang/glog"
	"github.com/spf13/afero"

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

func (p *progloader) LoadProgs(program_path string) (*Engine, int) {
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
		if filepath.Ext(fi.Name()) != fileext {
			continue
		}
		errors += p.LoadProg(program_path, fi.Name())
	}
	return &p.E, errors
}

func (p *progloader) LoadProg(program_path string, name string) (errors int) {
	pth := path.Join(program_path, name)
	f, err := p.fs.Open(pth)
	if err != nil {
		glog.Infof("Failed to read program %q: %s", pth, err)
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
	return
}

type progloader struct {
	sync.RWMutex
	w         watcher.Watcher
	pathnames map[string]struct{}
	E         Engine
	ms        *metrics.Store
	fs        afero.Fs
}

func NewProgLoader(w watcher.Watcher) (p *progloader) {
	p = &progloader{w: w,
		E:  make(map[string]*VM),
		fs: afero.OsFs{}}
	p.Lock()
	p.pathnames = make(map[string]struct{})
	p.Unlock()

	go p.start()
	return
}

func (p *progloader) start() {
	for {
		select {
		case name, more := <-p.w.Deletes():
			if !more {
				return
			}
			glog.Infof("delete prog")
			_, f := filepath.Split(name)
			p.E.RemoveVm(f)
			p.Lock()
			delete(p.pathnames, f)
			p.Unlock()
			if err := p.w.Remove(name); err != nil {
				glog.Info("Remove watch failed:", err)
			}

		case name, more := <-p.w.Creates():
			if !more {
				return
			}
			glog.Infof("create prog")
			if filepath.Ext(name) != fileext {
				continue
			}
			f := filepath.Base(name)

			p.Lock()
			if _, ok := p.pathnames[f]; !ok {
				p.pathnames[f] = struct{}{}
				p.w.Add(name)
			}
			p.Unlock()
		case name, more := <-p.w.Updates():
			if !more {
				return
			}
			glog.Infof("update prog")
			if filepath.Ext(name) != fileext {
				continue
			}
			d, f := filepath.Split(name)

			p.Lock()
			if _, ok := p.pathnames[f]; !ok {
				p.pathnames[f] = struct{}{}
				p.w.Add(name)
			}
			p.Unlock()
			p.LoadProg(d, f)
		}
	}
}
