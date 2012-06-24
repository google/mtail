// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"path/filepath"
	"regexp"
)

var compile_only *bool = flag.Bool("compile_only", false, "Compile programs only.")

type compiler struct {
	name   string   // Name of the program.
	errors []string // Compile errors.

	prog []instr          // The emitted program.
	str  []string         // Static strings.
	re   []*regexp.Regexp // Static regular expressions.

	symtab *scope
}

func Compile(name string, input io.Reader) (*vm, []string) {
	p := NewParser(name, input)
	r := EmtailParse(p)
	if r != 0 || p == nil || len(p.errors) > 0 {
		return nil, p.errors
	}
	if *compile_only {
		output := unparse(p.root)
		log.Printf("Unparsing %s:\n%s", name, output)
	}
	file := filepath.Base(name)
	c := &compiler{name: file, symtab: p.s}
	c.compile(p.root)
	if len(c.errors) > 0 {
		return nil, c.errors
	}
	vm := &vm{
		name:   file,
		re:     c.re,
		str:    c.str,
		symtab: c.symtab,
		prog:   c.prog}
	return vm, nil
}

func (c *compiler) errorf(format string, args ...interface{}) {
	e := fmt.Sprintf(c.name+": "+format, args...)
	c.errors = append(c.errors, e)
}

func (c *compiler) emit(i instr) {
	c.prog = append(c.prog, i)
}

func (c *compiler) compile(n node) {
	switch v := n.(type) {
	case *stmtlistNode:
		for _, child := range v.children {
			c.compile(child)
		}

	case *exprlistNode:
		for _, child := range v.children {
			c.compile(child)
		}

	case *declNode:
		v.sym.addr = len(metrics)
		metrics = append(metrics, v.m)

	case *condNode:
		// TODO(jaq): split off a new goroutine thread instead of doing conds serially.
		if v.cond != nil {
			c.compile(v.cond)
		}
		c.emit(instr{op: jnm})
		// Save PC of jump instruction
		pc := len(c.prog) - 1
		for _, child := range v.children {
			c.compile(child)
		}
		c.emit(instr{ret, 1})
		// rewrite jump target
		c.prog[pc].opnd = len(c.prog)

	case *regexNode:
		if v.re == nil {
			re, err := regexp.Compile(v.pattern)
			if err != nil {
				c.errorf("%s", err)
				return
			} else {
				c.re = append(c.re, re)
				v.re = re
				v.addr = len(c.re) - 1
			}
		}
		c.emit(instr{match, v.addr})

	case *stringNode:
		c.str = append(c.str, v.text)
		c.emit(instr{str, len(c.str) - 1})

	case *idNode:
		c.emit(instr{mload, v.sym.addr})
		m := v.sym.binding.(*Metric)
		if m.D == nil {
			c.emit(instr{dload, len(m.Keys)})
		}

	case *caprefNode:
		rn := v.sym.binding.(*regexNode)
		c.emit(instr{push, rn.addr})
		c.emit(instr{capref, v.sym.addr})

	case *builtinNode:
		if v.args != nil {
			c.compile(v.args)
			c.emit(instr{builtin[v.name], len(v.args.(*exprlistNode).children)})
		} else {
			c.emit(instr{op: builtin[v.name]})
		}

	case *additiveExprNode:
		c.compile(v.lhs)
		c.compile(v.rhs)
		switch v.op {
		case '+':
			c.emit(instr{op: add})
		case '-':
			c.emit(instr{op: sub})
		default:
			c.errorf("invalid op: %q\n", v.op)
		}

	case *assignExprNode:
		c.compile(v.lhs)
		c.compile(v.rhs)
		c.emit(instr{op: set})

	case *indexedExprNode:
		c.compile(v.index)
		c.compile(v.lhs)

	case *incExprNode:
		c.compile(v.lhs)
		c.emit(instr{op: inc})

	case *incByExprNode:
		c.compile(v.lhs)
		c.compile(v.rhs)
		c.emit(instr{inc, 1})

	case *constExprNode:
		c.emit(instr{push, v.value})

	default:
		c.errorf("undefined node type %T", n)
	}
}
