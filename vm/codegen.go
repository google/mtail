// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp"

	"github.com/google/mtail/metrics"
)

// compiler is data for the code generator.
type codegen struct {
	name string // Name of the program.

	errors ErrorList // Compile errors.
	obj    object    // The object to return

	decos []*decoNode // Decorator stack to unwind
}

// CodeGen is the function that compiles the program to bytecode and data.
func CodeGen(name string, ast node) (*object, error) {
	c := &codegen{name: name}
	Walk(c, ast)
	if len(c.errors) > 0 {
		return nil, c.errors
	}
	return &c.obj, nil
}

func (c *codegen) errorf(format string, args ...interface{}) {
	e := "Internal compiler error: " + fmt.Sprintf(format, args...)
	c.errors.Add(position{filename: c.name}, e)
}

func (c *codegen) emit(i instr) {
	c.obj.prog = append(c.obj.prog, i)
}

func (c *codegen) VisitBefore(node node) Visitor {
	switch n := node.(type) {

	case *declNode:
		// Build the list of addressable metrics for this program, and set the symbol's address.
		n.sym.addr = len(c.obj.m)
		c.obj.m = append(c.obj.m, n.sym.binding.(*metrics.Metric))
		return nil

	case *condNode:
		if n.cond != nil {
			Walk(c, n.cond)
		}
		// Save PC of previous jump instruction emitted by the n.cond
		// compilation.  (See regexNode and relNode cases, which will emit a
		// jump as the last instr.)  This jump will skip over the truthNode.
		pc := len(c.obj.prog) - 1
		// Set matched flag false for children.
		c.emit(instr{setmatched, false})
		Walk(c, n.truthNode)
		// Re-set matched flag to true for rest of current block.
		c.emit(instr{setmatched, true})
		// Rewrite n.cond's jump target to jump to instruction after block.
		c.obj.prog[pc].opnd = len(c.obj.prog)
		// Now also emit the else clause, and a jump.
		if n.elseNode != nil {
			c.emit(instr{op: jmp})
			// Rewrite jump again to avoid this else-skipper just emitted.
			c.obj.prog[pc].opnd = len(c.obj.prog)
			// Now get the PC of the else-skipper just emitted.
			pc = len(c.obj.prog) - 1
			Walk(c, n.elseNode)
			// Rewrite else-skipper to the next PC.
			c.obj.prog[pc].opnd = len(c.obj.prog)
		}
		return nil

	case *regexNode:
		re, err := regexp.Compile(n.pattern)
		if err != nil {
			c.errorf("%s", err)
			return nil
		}
		c.obj.re = append(c.obj.re, re)
		// Store the location of this regular expression in the regexNode
		n.addr = len(c.obj.re) - 1
		c.emit(instr{match, n.addr})
		c.emit(instr{op: jnm})

	case *stringConstNode:
		c.obj.str = append(c.obj.str, n.text)
		c.emit(instr{str, len(c.obj.str) - 1})

	case *intConstNode:
		c.emit(instr{push, n.i})

	case *floatConstNode:
		c.emit(instr{push, n.f})

	case *idNode:
		c.emit(instr{mload, n.sym.addr})
		m := n.sym.binding.(*metrics.Metric)
		c.emit(instr{dload, len(m.Keys)})

	case *caprefNode:
		if n.sym == nil {
			c.errorf("No symbol defined for this capref: %s, aborting compilation.", n.name)
			return nil
		}
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

func (c *codegen) VisitAfter(node node) {
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
