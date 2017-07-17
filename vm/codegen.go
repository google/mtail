// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp"
	"time"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/metrics/datum"
)

// compiler is data for the code generator.
type codegen struct {
	name string // Name of the program.

	errors ErrorList // Compile errors.
	obj    object    // The object to return

	decos []*decoNode // Decorator stack to unwind
}

// CodeGen is the function that compiles the program to bytecode and data.
func CodeGen(name string, ast astNode) (*object, error) {
	c := &codegen{name: name}
	Walk(c, ast)
	if len(c.errors) > 0 {
		return nil, c.errors
	}
	return &c.obj, nil
}

func (c *codegen) errorf(pos *position, format string, args ...interface{}) {
	e := "Internal compiler error, aborting compilation: " + fmt.Sprintf(format, args...)
	c.errors.Add(pos, e)
}

func (c *codegen) emit(i instr) {
	c.obj.prog = append(c.obj.prog, i)
}

var kindMap = map[Type]datum.Type{
	Int:     metrics.Int,
	Float:   metrics.Float,
	Buckets: metrics.Buckets,
}

func (c *codegen) VisitBefore(node astNode) Visitor {
	switch n := node.(type) {

	case *declNode:
		var name string
		if n.exportedName != "" {
			name = n.exportedName
		} else {
			name = n.name
		}
		// If the Type is not in the map, then default to metrics.Int.  This is
		// a hack for metrics that no type can be inferred, retaining
		// historical behaviour.
		kind := kindMap[n.Type()]
		m := metrics.NewMetric(name, c.name, n.kind, kind, n.keys...)
		// Scalar counters can be initialized to zero.  Dimensioned counters we
		// don't know the values of the labels yet.  Gauges and Timers we can't
		// assume start at zero.
		if len(n.keys) == 0 && n.kind == metrics.Counter {
			d, err := m.GetDatum()
			if err != nil {
				c.errorf(n.Pos(), "%s", err)
				return nil
			}
			// Initialize to zero at the zero time.
			if kind == metrics.Int {
				datum.SetInt(d, 0, time.Unix(0, 0))
			} else {
				datum.SetFloat(d, 0, time.Unix(0, 0))
			}
		}

		if n.kind == metrics.Histogram {
			if len(n.buckets) < 2 {
				c.errorf(n.Pos(), "a histogram need at least two boundaries")
				return nil
			}
			if n.buckets[0] >= n.buckets[1] {
				c.errorf(n.Pos(), "buckets boundaries must be sorted")
				return nil
			}

			ranges := make([]datum.Range, 0)
			ranges = append(ranges, datum.Range{n.buckets[0], n.buckets[1]})

			for _, max := range n.buckets[2:] {
				min := ranges[len(ranges)-1].Max
				if max <= min {
					c.errorf(n.Pos(), "buckets boundaries must be sorted")
					return nil
				}

				ranges = append(ranges, datum.Range{min, max})
			}

			for _, r := range ranges {
				m.Buckets = append(m.Buckets, r)
			}
		}

		m.Hidden = n.hidden
		(*n.sym).Binding = m
		n.sym.Addr = len(c.obj.m)
		c.obj.m = append(c.obj.m, m)
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
			c.errorf(n.Pos(), "%s", err)
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
		if n.sym == nil || n.sym.Binding == nil {
			c.errorf(n.Pos(), "No metric bound to identifier %q", n.name)
			return nil
		}
		c.emit(instr{mload, n.sym.Addr})
		m := n.sym.Binding.(*metrics.Metric)
		c.emit(instr{dload, len(m.Keys)})

	case *caprefNode:
		if n.sym == nil || n.sym.Binding == nil {
			c.errorf(n.Pos(), "No regular expression bound to capref %q", n.name)
			return nil
		}
		rn := n.sym.Binding.(*regexNode)
		// rn.addr contains the index of the regular expression object,
		// which correlates to storage on the re slice
		c.emit(instr{push, rn.addr})
		// n.sym.addr is the capture group offset
		c.emit(instr{capref, n.sym.Addr})

	case *defNode:
		// Do nothing, defs are inlined.
		return nil

	case *decoNode:
		// Put the current block on the stack
		c.decos = append(c.decos, n)
		if n.def == nil {
			c.errorf(n.Pos(), "No definition found for decorator %q", n.name)
			return nil
		}
		// then iterate over the decorator's nodes
		Walk(c, n.def.block)
		c.decos = c.decos[:len(c.decos)-1]
		return nil

	case *nextNode:
		// Visit the 'next' block on the decorated block stack
		deco := c.decos[len(c.decos)-1]
		Walk(c, deco.block)
		return nil

	case *otherwiseNode:
		c.emit(instr{op: otherwise})
		c.emit(instr{op: jnm})

	case *delNode:
		Walk(c, n.n)
		// overwdrite the dload instruction
		pc := len(c.obj.prog) - 1
		c.obj.prog[pc].op = del
	}

	return c
}

var typedOperators = map[int]map[Type]opcode{
	PLUS: {Int: iadd,
		Float: fadd},
	MINUS: {Int: isub,
		Float: fsub},
	MUL: {Int: imul,
		Float: fmul},
	DIV: {Int: idiv,
		Float: fdiv},
	MOD: {Int: imod,
		Float: fmod},
	POW: {Int: ipow,
		Float: fpow},
	ASSIGN: {Int: iset,
		Float: fset},
}

func (c *codegen) VisitAfter(node astNode) {
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
		case PLUS, MINUS, MUL, DIV, MOD, POW, ASSIGN:
			switch n.Type() {
			case Int, Float:
				c.emit(instr{op: typedOperators[n.op][n.Type()]})
			default:
				c.errorf(n.Pos(), "Invalid type for binary expression: %q", n.Type())
			}
		case AND:
			c.emit(instr{op: and})
		case OR:
			c.emit(instr{op: or})
		case XOR:
			c.emit(instr{op: xor})
		case SHL:
			c.emit(instr{op: shl})
		case SHR:
			c.emit(instr{op: shr})
		}
	}
}
