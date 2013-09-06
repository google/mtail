package main

import (
	"exp/inotify"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"text/tabwriter"
)

func (p *progloader) LoadProgs(program_path string) (*engine, int) {
	fis, err := ioutil.ReadDir(program_path)
	if err != nil {
		log.Fatalf("Failed to list programs in %q: %s", program_path, err)
	}

	e := &engine{}
	errors := 0
	for _, fi := range fis {
		if fi.IsDir() {
			continue
		}
		if filepath.Ext(fi.Name()) != ".em" {
			continue
		}
		f, err := os.Open(fmt.Sprintf("%s/%s", program_path, fi.Name()))
		if err != nil {
			log.Printf("Failed to read program %q: %s\n", fi.Name(), err)
			continue
		}
		defer f.Close()
		v, errs := Compile(fi.Name(), f)
		if errs != nil {
			errors = 1
			for _, e := range errs {
				log.Print(e)
			}
			continue
		}
		if *dump_bytecode {
			fmt.Printf("Prog %s\n", fi.Name())
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
		e.addVm(v)
		log.Printf("loaded %s", fi.Name())
	}
	return e, errors
}

type progloader struct {
	w         Watcher
	pathnames map[string]struct{}
}

func NewProgLoader(w Watcher) (p *progloader) {
	p = &progloader{w: w,
		pathnames: make(map[string]struct{})}
	return
}

var (
	tProgCreateMask = inotify.IN_CREATE
	tProgChangeMask = inotify.IN_MODIFY
)

func (p *progloader) start() {
	for {
		select {
		case ev := <-p.w.Events():
			switch {
			case ev.Mask&tProgCreateMask != 0:
				if filepath.Ext(ev.Name) != ".em" {
					continue
				}
				p.pathnames[ev.Name] = struct{}{}
			case ev.Mask&tProgChangeMask != 0:
				// reload program
			}
		case err := <-p.w.Errors():
			log.Println("watch error: ", err)

		}
	}
}
