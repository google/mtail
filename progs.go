package main

import (
	"code.google.com/p/go.exp/inotify"
	"expvar"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"text/tabwriter"
)

var (
	prog_loads       = expvar.NewMap("prog_loads_total")
	prog_load_errors = expvar.NewMap("prog_load_errors")
)

func (p *progloader) LoadProgs(program_path string) (*engine, int) {
	p.w.AddWatch(program_path, tProgCreateMask)
	log.Println("Watch on ", program_path, " with ", tProgCreateMask)

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
	f, err := os.Open(path.Join(program_path, name))
	if err != nil {
		log.Printf("Failed to read program %q: %s\n", name, err)
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
	log.Printf("loaded %s", name)
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
	w         Watcher
	pathnames map[string]struct{}
	e         engine
}

func NewProgLoader(w Watcher) (p *progloader) {
	p = &progloader{w: w,
		pathnames: make(map[string]struct{}),
		e:         make(map[string]*vm)}
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
			switch {
			case ev.Mask&tProgCreateMask|tProgChangeMask != 0:
				log.Println("File: ", ev.Name)
				if filepath.Ext(ev.Name) != ".em" {
					continue
				}
				d, f := filepath.Split(ev.Name)
				if _, ok := p.pathnames[f]; !ok {
					p.pathnames[f] = struct{}{}
					p.w.AddWatch(ev.Name, tProgChangeMask|tProgDeleteMask)
				}
				p.LoadProg(d, f)

			case ev.Mask&tProgDeleteMask != 0:
				_, f := filepath.Split(ev.Name)
				p.e.removeVm(f)
				delete(p.pathnames, f)
				if err := p.w.RemoveWatch(ev.Name); err != nil {
					log.Println("Remove watch failed:", err)
				}
			}
		case err := <-p.w.Errors():
			log.Println("watch error: ", err)

		}
	}
}
