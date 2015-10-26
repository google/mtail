// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Build the parser:
//go:generate go tool yacc -v y.output -o parser.go -p mtail parser.y

package vm

import (
	"fmt"
	"io"
	"path/filepath"
	"regexp"

	"github.com/google/mtail/metrics"
)

type compiler struct {
	name string // Name of the program.

	errors ErrorList         // Compile errors.
	prog   []instr           // The emitted program.
	str    []string          // Static strings.
	re     []*regexp.Regexp  // Static regular expressions.
	m      []*metrics.Metric // Metrics accessible to this program.

	decos []*decoNode // Decorator stack to unwind

	symtab *scope
}

// Compile compiles a program from the input into a virtual machine or a list
// of compile errors.  It takes the program's name and the metric store as
// additional arguments to build the virtual machine.
func Compile(name string, input io.Reader, ms *metrics.Store, compileOnly bool, syslogUseCurrentYear bool) (*VM, error) {
	name = filepath.Base(name)
	p := newParser(name, input, ms)
	r := mtailParse(p)
	if r != 0 || p == nil || p.errors != nil {
		return nil, p.errors
	}
	c := &compiler{name: name, symtab: p.s}
	c.compile(p.root)
	if len(c.errors) > 0 {
		return nil, c.errors
	}
	if compileOnly {
		return nil, nil
	}

	vm := New(name, c.re, c.str, c.m, c.prog, syslogUseCurrentYear)
	return vm, nil
}

func (c *compiler) errorf(format string, args ...interface{}) {
	e := fmt.Sprintf(format, args...)
	c.errors.Add(position{filename: c.name}, e)
}

func (c *compiler) emit(i instr) {
	c.prog = append(c.prog, i)
}

func (c *compiler) compile(untypedNode node) {
	switch n := untypedNode.(type) {
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
		c.m = append(c.m, n.sym.binding.(*metrics.Metric))

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
			}
			c.re = append(c.re, re)
			n.re = re
			// Store the location of this regular expression in the regexNode
			n.addr = len(c.re) - 1
		}
		c.emit(instr{match, n.addr})
		c.emit(instr{op: jnm})

	case *binaryExprNode:
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
		case '+':
			c.emit(instr{op: add})
		case '-':
			c.emit(instr{op: sub})
		case '*':
			c.emit(instr{op: mul})
		case '/':
			c.emit(instr{op: div})
		case AND:
			c.emit(instr{op: and})
		case OR:
			c.emit(instr{op: or})
		case XOR:
			c.emit(instr{op: xor})
		case ASSIGN:
			c.emit(instr{op: set})
		case ADD_ASSIGN:
			c.emit(instr{inc, 1})
		case SHL:
			c.emit(instr{op: shl})
		case SHR:
			c.emit(instr{op: shr})
		case POW:
			c.emit(instr{op: pow})
		}

	case *unaryExprNode:
		c.compile(n.lhs)
		switch n.op {
		case INC:
			c.emit(instr{op: inc})
		case NOT:
			c.emit(instr{op: not})
		}

	case *indexedExprNode:
		c.compile(n.index)
		c.compile(n.lhs)

	case *numericExprNode:
		c.emit(instr{push, int(n.i)})

	case *stringNode:
		c.str = append(c.str, n.text)
		c.emit(instr{str, len(c.str) - 1})

	case *idNode:
		c.emit(instr{mload, n.sym.addr})
		m := n.sym.binding.(*metrics.Metric)
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
		c.errorf("undefined node type %T (%q)6", untypedNode, untypedNode)
	}
}
