package main

// emtail programs may be updated while emtail is running, and they will be
// reloaded without having to restart the emtail process. Programs can be
// created and deleted as well, and some configuration systems do an atomic
// rename of the program when it is installed, so emtail is also aware of file
// moves.

import (
	"code.google.com/p/go.exp/inotify"
	"expvar"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"sync"
	"text/tabwriter"
)

var (
	prog_ev_count    = expvar.NewInt("prog_events_total")
	prog_loads       = expvar.NewMap("prog_loads_total")
	prog_load_errors = expvar.NewMap("prog_load_errors")
)

func (p *progloader) LoadProgs(program_path string) (*engine, int) {
	p.w.AddWatch(program_path, tProgCreateMask)

	fis, err := ioutil.ReadDir(program_path)
	if err != nil {
		log.Fatalf("Failed to list programs in %q: %s", program_path, err)
	}

	errors := 0
	for _, fi := range fis {
		if fi.IsDir() {
			continue
		}
		if filepath.Ext(fi.Name()) != ".em" {
			continue
		}
		errors += p.LoadProg(program_path, fi.Name())
	}
	return &p.e, errors
}

func (p *progloader) LoadProg(program_path string, name string) (errors int) {
	pth := path.Join(program_path, name)
	f, err := os.Open(pth)
	if err != nil {
		log.Printf("Failed to read program %q: %s\n", pth, err)
		errors = 1
		prog_load_errors.Add(name, 1)
		return
	}
	defer f.Close()
	v, errs := Compile(name, f)
	if errs != nil {
		errors = 1
		for _, e := range errs {
			log.Print(e)
		}
		prog_load_errors.Add(name, 1)
		return
	}
	if *dump_bytecode {
		p.DumpByteCode(name, v)
	}
	p.e.addVm(name, v)
	prog_loads.Add(name, 1)
	return
}

func (p *progloader) DumpByteCode(name string, v *vm) {
	fmt.Printf("Prog %s\n", name)
	fmt.Println("Metrics")
	for i, m := range metrics {
		if m.Program == v.name {
			fmt.Printf(" %8d %s\n", i, m)
		}
	}
	fmt.Println("REs")
	for i, re := range v.re {
		fmt.Printf(" %8d /%s/\n", i, re)
	}
	w := new(tabwriter.Writer)
	w.Init(os.Stdout, 0, 0, 1, ' ', tabwriter.AlignRight)

	fmt.Fprintln(w, "disasm\tl\top\topnd\t")
	for n, i := range v.prog {
		fmt.Fprintf(w, "\t%d\t%s\t%d\t\n", n, opNames[i.op], i.opnd)
	}
	w.Flush()
}

type progloader struct {
	sync.RWMutex
	w         Watcher
	pathnames map[string]struct{}
	e         engine
}

func NewProgLoader(w Watcher) (p *progloader) {
	p = &progloader{w: w,
		e: make(map[string]*vm)}
	p.Lock()
	p.pathnames = make(map[string]struct{})
	p.Unlock()

	go p.start()
	return
}

var (
	tProgCreateMask = inotify.IN_CREATE | inotify.IN_ONLYDIR
	tProgChangeMask = inotify.IN_MODIFY
	tProgDeleteMask = inotify.IN_DELETE
)

func (p *progloader) start() {
	for {
		select {
		case ev := <-p.w.Events():
			prog_ev_count.Add(1)
			switch {
			case ev.Mask&tProgDeleteMask != 0:
				_, f := filepath.Split(ev.Name)
				p.e.removeVm(f)
				p.Lock()
				delete(p.pathnames, f)
				p.Unlock()
				if err := p.w.RemoveWatch(ev.Name); err != nil {
					log.Println("Remove watch failed:", err)
				}

			case ev.Mask&tProgCreateMask|tProgChangeMask != 0:
				if filepath.Ext(ev.Name) != ".em" {
					continue
				}
				d, f := filepath.Split(ev.Name)

				p.Lock()
				if _, ok := p.pathnames[f]; !ok {
					p.pathnames[f] = struct{}{}
					p.w.AddWatch(ev.Name, tProgChangeMask|tProgDeleteMask)
				}
				p.Unlock()
				p.LoadProg(d, f)

			default:
				log.Printf("Unknown event: %q", ev)
			}
		case err := <-p.w.Errors():
			log.Println("watch error: ", err)

		}
	}
}
