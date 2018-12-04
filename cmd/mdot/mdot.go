// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

/*
Command mdot turns an mtail program AST into a graphviz graph on standard output.

To use, run it like

  go run github.com/google/mtail/cmd/mdot --prog ../../examples/dhcpd.mtail | xdot -
*/
package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/vm/ast"
	"github.com/google/mtail/internal/vm/checker"
	"github.com/google/mtail/internal/vm/parser"
)

var (
	prog = flag.String("prog", "", "Name of the program source to parse.")
)

type dotter struct {
	id       int
	parentID []int // id of the parent node
}

func (d *dotter) nextID() int {
	d.id++
	return d.id
}

func (d *dotter) emitNode(id int, node ast.Node) {
	attrs := map[string]string{
		"label": strings.Split(fmt.Sprintf("%T", node), ".")[1] + "\n",
		"shape": "box",
		"style": "filled",
	}
	switch n := node.(type) {
	case *ast.VarDecl, *ast.DecoDecl:
		attrs["fillcolor"] = "green"
		switch n := n.(type) {
		case *ast.VarDecl:
			attrs["label"] += fmt.Sprintf("%s %s", n.Kind, n.Name)
		case *ast.DecoDecl:
			attrs["label"] += n.Name
		}
	case *ast.IdTerm, *ast.CaprefTerm:
		attrs["fillcolor"] = "pink"
		attrs["shape"] = "ellipse"
		switch n := n.(type) {
		case *ast.IdTerm:
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
		}
	}
	pos := node.Pos()
	if pos != nil {
		attrs["xlabel"] = pos.String()
	}
	fmt.Printf("%d [", id)
	for k, v := range attrs {
		fmt.Printf("%s=\"%s\" ", k, v)
	}
	fmt.Printf("]\n")
}

func (d *dotter) emitLine(src, dst int) {
	fmt.Printf("%d -> %d\n", src, dst)
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

func main() {
	flag.Parse()

	if *prog == "" {
		glog.Exitf("No -prog given")
	}

	f, err := os.Open(*prog)
	if err != nil {
		glog.Fatal(err)
	}
	n, err := parser.Parse(*prog, f)
	if err != nil {
		glog.Exit(err)
	}
	n, err = checker.Check(n)
	if err != nil {
		glog.Exit(err)
	}
	dot := &dotter{}
	fmt.Printf("digraph \"%s\" {\n", *prog)
	ast.Walk(dot, n)
	fmt.Println("}")
}
