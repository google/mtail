// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

/*
Command mdot turns an mtail program AST into a graphviz graph on standard output.

To use, run it like (assuming your shell is in the same directory as this file)

	go run github.com/jaqx0r/mtail/cmd/mdot --prog ../../examples/dhcpd.mtail | xdot -

or

	go run github.com/jaqx0r/mtail/cmd/mdot --prog ../../examples/dhcpd.mtail --http_port 8080

to view the dot output visit http://localhost:8080

You'll need the graphviz `dot' command installed.
*/
package main

import (
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/mtail"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/checker"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
)

var (
	prog     = flag.String("prog", "", "Name of the program source to parse.")
	httpPort = flag.String("http_port", "", "Port number to run HTTP server on.")
)

type dotter struct {
	w        io.Writer
	id       int
	parentID []int // id of the parent node
}

func (d *dotter) nextID() int {
	d.id++
	return d.id
}

func (d *dotter) emitNode(id int, node ast.Node) {
	attrs := map[string]string{
		"label":   strings.Split(fmt.Sprintf("%T", node), ".")[1] + "\n",
		"shape":   "box",
		"style":   "filled",
		"tooltip": node.Type().String(),
	}
	switch n := node.(type) {
	case *ast.VarDecl, *ast.DecoDecl:
		attrs["fillcolor"] = "lightgreen"
		switch n := n.(type) {
		case *ast.VarDecl:
			attrs["label"] += fmt.Sprintf("%s %s", n.Kind, n.Name)
		case *ast.DecoDecl:
			attrs["label"] += n.Name
		}
	case *ast.IDTerm, *ast.CaprefTerm:
		attrs["fillcolor"] = "pink"
		attrs["shape"] = "ellipse"
		switch n := n.(type) {
		case *ast.IDTerm:
			attrs["label"] += n.Name
		case *ast.CaprefTerm:
			attrs["label"] += fmt.Sprintf("$%s", n.Name)
		}
	case *ast.IntLit, *ast.FloatLit, *ast.PatternLit, *ast.StringLit:
		attrs["fillcolor"] = "pink"
		attrs["shape"] = "ellipse"
		switch n := n.(type) {
		case *ast.IntLit:
			attrs["label"] += fmt.Sprintf("%d", n.I)
		case *ast.FloatLit:
			attrs["label"] += fmt.Sprintf("%g", n.F)
		case *ast.PatternLit:
			attrs["label"] += fmt.Sprintf("/%s/", n.Pattern)
		case *ast.StringLit:
			attrs["label"] += n.Text
		}
	case *ast.IndexedExpr, *ast.BinaryExpr, *ast.UnaryExpr, *ast.PatternExpr, *ast.BuiltinExpr:
		attrs["fillcolor"] = "lightblue"
		switch n := n.(type) {
		case *ast.BinaryExpr:
			attrs["label"] += parser.Kind(n.Op).String()
		case *ast.UnaryExpr:
			attrs["label"] += parser.Kind(n.Op).String()
		case *ast.BuiltinExpr:
			attrs["label"] += n.Name
		}
	}
	pos := node.Pos()
	if pos != nil {
		attrs["xlabel"] = pos.String()
	}
	fmt.Fprintf(d.w, "n%d [", id)
	for k, v := range attrs {
		fmt.Fprintf(d.w, "%s=\"%s\" ", k, v)
	}
	fmt.Fprintf(d.w, "]\n")
}

func (d *dotter) emitLine(src, dst int) {
	fmt.Fprintf(d.w, "n%d -> n%d\n", src, dst)
}

func (d *dotter) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	id := d.nextID()
	d.emitNode(id, node)
	if len(d.parentID) > 0 {
		parentID := d.parentID[len(d.parentID)-1]
		d.emitLine(parentID, id)
	}
	d.parentID = append(d.parentID, id)
	return d, node
}

func (d *dotter) VisitAfter(node ast.Node) ast.Node {
	d.parentID = d.parentID[:len(d.parentID)-1]
	return node
}

func makeDot(name string, w io.Writer) error {
	f, err := os.Open(filepath.Clean(name))
	if err != nil {
		return err
	}
	n, err := parser.Parse(name, f)
	if err != nil {
		return err
	}
	n, err = checker.Check(n, 0, 0)
	if err != nil {
		return err
	}
	fmt.Fprintf(w, "digraph \"%s\" {\n", *prog)
	dot := &dotter{w: w}
	ast.Walk(dot, n)
	fmt.Fprintf(w, "}\n")
	return nil
}

func main() {
	flag.Parse()

	if *prog == "" {
		glog.Exitf("No -prog given")
	}

	if *httpPort == "" {
		glog.Exit(makeDot(*prog, os.Stdout))
	}

	http.HandleFunc("/",
		func(w http.ResponseWriter, _ *http.Request) {
			dot := exec.Command("dot", "-Tsvg")
			in, err := dot.StdinPipe()
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			out, err := dot.StdoutPipe()
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			err = dot.Start()
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			err = makeDot(*prog, in)
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			err = in.Close()
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			w.Header().Add("Content-type", "image/svg+xml")
			w.WriteHeader(http.StatusOK)
			_, err = io.Copy(w, out)
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
			}
			err = dot.Wait()
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
			}
		})
	http.HandleFunc("/favicon.ico", mtail.FaviconHandler)
	glog.Info(http.ListenAndServe(fmt.Sprintf(":%s", *httpPort), nil))
}
