// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"fmt"
	"io"
	"path/filepath"
	"regexp"
)

var compile_only *bool = flag.Bool("compile_only", false, "Compile programs only.")

type compiler struct {
	name string // Name of the program.

	errors []string         // Compile errors.
	prog   []instr          // The emitted program.
	str    []string         // Static strings.
	re     []*regexp.Regexp // Static regular expressions.
	m      []*Metric        // Metrics accessible to this program.

	decos []*decoNode // Decorator stack to unwind

	symtab *scope
}

func Compile(name string, input io.Reader) (*vm, []string) {
	name = filepath.Base(name)
	p := NewParser(name, input)
	r := EmtailParse(p)
	if r != 0 || p == nil || len(p.errors) > 0 {
		return nil, p.errors
	}
	if *compile_only {
		u := Unparser{}
		output := u.Unparse(p.root)
		fmt.Printf("Unparsing %s:\n%s", name, output)
	}
	c := &compiler{name: name, symtab: p.s}
	c.compile(p.root)
	if len(c.errors) > 0 {
		return nil, c.errors
	}

	vm := newVm(name, c.re, c.str, c.m, c.prog)
	return vm, nil
}

func (c *compiler) errorf(format string, args ...interface{}) {
	e := fmt.Sprintf(c.name+": "+format, args...)
	c.errors = append(c.errors, e)
}

func (c *compiler) emit(i instr) {
	c.prog = append(c.prog, i)
}

func (c *compiler) compile(untyped_node node) {
	switch n := untyped_node.(type) {
	case *stmtlistNode:
		for _, child := range n.children {
			c.compile(child)
		}

	case *exprlistNode:
		for _, child := range n.children {
			c.compile(child)
		}

	case *declNode:
		// Build the list of addressable metrics for this program, and set the symbol's address.
		n.sym.addr = len(c.m)
		c.m = append(c.m, n.sym.binding.(*Metric))

	case *condNode:
		if n.cond != nil {
			c.compile(n.cond)
		}
		// Save PC of previous jump instruction
		// (see regexNode and relNode cases, which will emit a jump)
		pc := len(c.prog) - 1
		for _, child := range n.children {
			c.compile(child)
		}
		// Rewrite jump target to jump to instruction after block.
		c.prog[pc].opnd = len(c.prog)

	case *regexNode:
		if n.re == nil {
			re, err := regexp.Compile(n.pattern)
			if err != nil {
				c.errorf("%s", err)
				return
			} else {
				c.re = append(c.re, re)
				n.re = re
				// Store the location of this regular expression in the regexNode
				n.addr = len(c.re) - 1
			}
		}
		c.emit(instr{match, n.addr})
		c.emit(instr{op: jnm})

	case *relNode:
		c.compile(n.lhs)
		c.compile(n.rhs)
		switch n.op {
		case LT:
			c.emit(instr{cmp, -1})
			c.emit(instr{op: jnm})
		case GT:
			c.emit(instr{cmp, 1})
			c.emit(instr{op: jnm})
		case LE:
			c.emit(instr{cmp, 1})
			c.emit(instr{op: jm})
		case GE:
			c.emit(instr{cmp, -1})
			c.emit(instr{op: jm})
		case EQ:
			c.emit(instr{cmp, 0})
			c.emit(instr{op: jnm})
		case NE:
			c.emit(instr{cmp, 0})
			c.emit(instr{op: jm})
		default:
			c.errorf("invalid op: %q\n", n.op)
		}

	case *stringNode:
		c.str = append(c.str, n.text)
		c.emit(instr{str, len(c.str) - 1})

	case *idNode:
		c.emit(instr{mload, n.sym.addr})
		m := n.sym.binding.(*Metric)
		c.emit(instr{dload, len(m.Keys)})

	case *caprefNode:
		rn := n.sym.binding.(*regexNode)
		// rn.addr contains the index of the regular expression object,
		// which correlates to storage on the re heap
		c.emit(instr{push, rn.addr})
		c.emit(instr{capref, n.sym.addr})

	case *builtinNode:
		if n.args != nil {
			c.compile(n.args)
			c.emit(instr{builtin[n.name], len(n.args.(*exprlistNode).children)})
		} else {
			c.emit(instr{op: builtin[n.name]})
		}

	case *additiveExprNode:
		c.compile(n.lhs)
		c.compile(n.rhs)
		switch n.op {
		case '+':
			c.emit(instr{op: add})
		case '-':
			c.emit(instr{op: sub})
		default:
			c.errorf("invalid op: %q\n", n.op)
		}

	case *assignExprNode:
		c.compile(n.lhs)
		c.compile(n.rhs)
		c.emit(instr{op: set})

	case *indexedExprNode:
		c.compile(n.index)
		c.compile(n.lhs)

	case *incExprNode:
		c.compile(n.lhs)
		c.emit(instr{op: inc})

	case *incByExprNode:
		c.compile(n.lhs)
		c.compile(n.rhs)
		c.emit(instr{inc, 1})

	case *numericExprNode:
		c.emit(instr{push, n.value})

	case *defNode:
		// Do nothing, defs are inlined.

	case *decoNode:
		// Put the current block on the stack
		c.decos = append(c.decos, n)
		// then iterate over the decorator's nodes
		for _, child := range n.def.children {
			c.compile(child)
		}
		// Pop the block off
		c.decos = c.decos[:len(c.decos)-1]

	case *nextNode:
		// Visit the 'next' block on the decorated block stack
		deco := c.decos[len(c.decos)-1]
		for _, child := range deco.children {
			c.compile(child)
		}

	default:
		c.errorf("undefined node type %T (%q)6", untyped_node, untyped_node)
	}
}
