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

// Options contains all the parameters that affect the behaviour of the compiler.
type Options struct {
	CompileOnly          bool // Do not start the program after compilation.
	SyslogUseCurrentYear bool // Use the current year if no year is present in the log file timestamp.
}

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
func Compile(name string, input io.Reader, ms *metrics.Store, o *Options) (*VM, error) {
	name = filepath.Base(name)
	p := newParser(name, input, ms)
	r := mtailParse(p)
	if r != 0 || p == nil || p.errors != nil {
		return nil, p.errors
	}
	c := &compiler{name: name, symtab: p.s}
	Walk(c, p.root)
	if len(c.errors) > 0 {
		return nil, c.errors
	}
	if err := Check(p.root); err != nil {
		return nil, err
	}

	if o.CompileOnly {
		return nil, nil
	}

	vm := New(name, c.re, c.str, c.m, c.prog, o.SyslogUseCurrentYear)
	return vm, nil
}

func (c *compiler) errorf(format string, args ...interface{}) {
	e := fmt.Sprintf(format, args...)
	c.errors.Add(position{filename: c.name}, e)
}

func (c *compiler) emit(i instr) {
	c.prog = append(c.prog, i)
}

func (c *compiler) VisitBefore(node node) Visitor {
	switch n := node.(type) {

	case *declNode:
		// Build the list of addressable metrics for this program, and set the symbol's address.
		n.sym.addr = len(c.m)
		c.m = append(c.m, n.sym.binding.(*metrics.Metric))
		return nil

	case *condNode:
		if n.cond != nil {
			Walk(c, n.cond)
		}
		// Save PC of previous jump instruction emitted by the n.cond
		// compilation.  (See regexNode and relNode cases, which will emit a
		// jump as the last instr.)  This jump will skip over the truthNode.
		pc := len(c.prog) - 1
		// Set matched flag false for children.
		c.emit(instr{setmatched, false})
		Walk(c, n.truthNode)
		// Re-set matched flag to true for rest of current block.
		c.emit(instr{setmatched, true})
		// Rewrite n.cond's jump target to jump to instruction after block.
		c.prog[pc].opnd = len(c.prog)
		// Now also emit the else clause, and a jump.
		if n.elseNode != nil {
			c.emit(instr{op: jmp})
			// Rewrite jump again to avoid this else-skipper just emitted.
			c.prog[pc].opnd = len(c.prog)
			// Now get the PC of the else-skipper just emitted.
			pc = len(c.prog) - 1
			Walk(c, n.elseNode)
			// Rewrite else-skipper to the next PC.
			c.prog[pc].opnd = len(c.prog)
		}
		return nil

	case *regexNode:
		re, err := regexp.Compile(n.pattern)
		if err != nil {
			c.errorf("%s", err)
			return nil
		}
		c.re = append(c.re, re)
		// Store the location of this regular expression in the regexNode
		n.addr = len(c.re) - 1
		c.emit(instr{match, n.addr})
		c.emit(instr{op: jnm})

	case *stringConstNode:
		c.str = append(c.str, n.text)
		c.emit(instr{str, len(c.str) - 1})

	case *intConstNode:
		c.emit(instr{push, n.i})

	case *floatConstNode:
		c.emit(instr{push, n.f})

	case *idNode:
		c.emit(instr{mload, n.sym.addr})
		m := n.sym.binding.(*metrics.Metric)
		c.emit(instr{dload, len(m.Keys)})

	case *caprefNode:
		rn := n.sym.binding.(*regexNode)
		// rn.addr contains the index of the regular expression object,
		// which correlates to storage on the re slice
		c.emit(instr{push, rn.addr})
		// n.sym.addr is the capture group offset
		c.emit(instr{capref, n.sym.addr})

	case *defNode:
		// Do nothing, defs are inlined.
		return nil

	case *decoNode:
		// Put the current block on the stack
		c.decos = append(c.decos, n)
		// then iterate over the decorator's nodes
		walknodelist(c, n.def.children)
		c.decos = c.decos[:len(c.decos)-1]
		return nil

	case *nextNode:
		// Visit the 'next' block on the decorated block stack
		deco := c.decos[len(c.decos)-1]
		walknodelist(c, deco.children)
		return nil

	case *otherwiseNode:
		c.emit(instr{op: otherwise})
		c.emit(instr{op: jnm})
	}

	return c
}

func (c *compiler) VisitAfter(node node) {
	switch n := node.(type) {
	case *builtinNode:
		if n.args != nil {
			c.emit(instr{builtin[n.name], len(n.args.(*exprlistNode).children)})
		} else {
			c.emit(instr{op: builtin[n.name]})
		}
	case *unaryExprNode:
		switch n.op {
		case INC:
			c.emit(instr{op: inc})
		case NOT:
			c.emit(instr{op: not})
		}
	case *binaryExprNode:
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
		case '%':
			c.emit(instr{op: mod})
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
	}
}
