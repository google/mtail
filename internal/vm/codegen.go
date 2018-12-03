// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/vm/ast"
	"github.com/google/mtail/internal/vm/bytecode"
	"github.com/google/mtail/internal/vm/errors"
	"github.com/google/mtail/internal/vm/parser"
	"github.com/google/mtail/internal/vm/position"
	"github.com/google/mtail/internal/vm/symtab"
	"github.com/google/mtail/internal/vm/types"
)

// codegen represents a code generator.
type codegen struct {
	name string // Name of the program.

	errors errors.ErrorList // Any compile errors detected are accumulated here.
	obj    object           // The object to return, if successful.

	l     []int           // Label table for recording jump destinations.
	decos []*ast.DecoNode // Decorator stack to unwind when entering decorated blocks.
}

// CodeGen is the function that compiles the program to bytecode and data.
func CodeGen(name string, n ast.Node) (*object, error) {
	c := &codegen{name: name}
	_ = ast.Walk(c, n)
	c.writeJumps()
	if len(c.errors) > 0 {
		return nil, c.errors
	}
	return &c.obj, nil
}

func (c *codegen) errorf(pos *position.Position, format string, args ...interface{}) {
	e := "Internal compiler error, aborting compilation: " + fmt.Sprintf(format, args...)
	c.errors.Add(pos, e)
}

func (c *codegen) emit(i bytecode.Instr) {
	c.obj.prog = append(c.obj.prog, i)
}

// newLabel creates a new label to jump to
func (c *codegen) newLabel() (l int) {
	l = len(c.l)
	c.l = append(c.l, -1)
	return
}

// setLabel points a label to the next instruction
func (c *codegen) setLabel(l int) {
	c.l[l] = c.pc() + 1
}

// pc returns the program offset of the last instruction
func (c *codegen) pc() int {
	return len(c.obj.prog) - 1
}

func (c *codegen) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	switch n := node.(type) {

	case *ast.DeclNode:
		var name string
		if n.ExportedName != "" {
			name = n.ExportedName
		} else {
			name = n.Name
		}
		// If the Type is not in the map, then default to metrics.Int.  This is
		// a hack for metrics that no type can be inferred, retaining
		// historical behaviour.
		t := n.Type()
		if types.IsDimension(t) {
			t = t.(*types.TypeOperator).Args[len(t.(*types.TypeOperator).Args)-1]
		}
		var dtyp datum.Type
		switch {
		case types.Equals(types.Float, t):
			dtyp = metrics.Float
		case types.Equals(types.String, t):
			dtyp = metrics.String
		default:
			if !types.IsComplete(t) {
				glog.Infof("Incomplete type %v for %#v", t, n)
			}
			dtyp = metrics.Int
		}
		m := metrics.NewMetric(name, c.name, n.Kind, dtyp, n.Keys...)
		m.SetSource(n.Pos().String())
		// Scalar counters can be initialized to zero.  Dimensioned counters we
		// don't know the values of the labels yet.  Gauges and Timers we can't
		// assume start at zero.
		if len(n.Keys) == 0 && n.Kind == metrics.Counter {
			d, err := m.GetDatum()
			if err != nil {
				c.errorf(n.Pos(), "%s", err)
				return nil, n
			}
			// Initialize to zero at the zero time.
			switch dtyp {
			case metrics.Int:
				datum.SetInt(d, 0, time.Unix(0, 0))
			case metrics.Float:
				datum.SetFloat(d, 0, time.Unix(0, 0))
			default:
				c.errorf(n.Pos(), "Can't initialize to zero a %v", n)
				return nil, n
			}
		}
		m.Hidden = n.Hidden
		(*n.Symbol).Binding = m
		n.Symbol.Addr = len(c.obj.m)
		c.obj.m = append(c.obj.m, m)
		return nil, n

	case *ast.Cond:
		lElse := c.newLabel()
		lEnd := c.newLabel()
		if n.Cond != nil {
			n.Cond = ast.Walk(c, n.Cond)
			c.emit(bytecode.Instr{bytecode.Jnm, lElse})
		}
		// Set matched flag false for children.
		c.emit(bytecode.Instr{bytecode.Setmatched, false})
		n.Truth = ast.Walk(c, n.Truth)
		// Re-set matched flag to true for rest of current block.
		c.emit(bytecode.Instr{bytecode.Setmatched, true})
		if n.Else != nil {
			c.emit(bytecode.Instr{bytecode.Jmp, lEnd})
		}
		c.setLabel(lElse)
		if n.Else != nil {
			n.Else = ast.Walk(c, n.Else)
		}
		c.setLabel(lEnd)
		return nil, n

	case *ast.PatternExpr:
		re, err := regexp.Compile(n.Pattern)
		if err != nil {
			c.errorf(n.Pos(), "%s", err)
			return nil, n
		}
		c.obj.re = append(c.obj.re, re)
		// Store the location of this regular expression in the patterNode
		n.Index = len(c.obj.re) - 1
		c.emit(bytecode.Instr{bytecode.Match, n.Index})

	case *ast.StringConst:
		c.obj.str = append(c.obj.str, n.Text)
		c.emit(bytecode.Instr{bytecode.Str, len(c.obj.str) - 1})

	case *ast.IntConst:
		c.emit(bytecode.Instr{bytecode.Push, n.I})

	case *ast.FloatConst:
		c.emit(bytecode.Instr{bytecode.Push, n.F})

	case *ast.StopNode:
		c.emit(bytecode.Instr{bytecode.Stop, nil})

	case *ast.Id:
		if n.Symbol == nil || n.Symbol.Kind != symtab.VarSymbol {
			break
		}
		if n.Symbol.Binding == nil {
			c.errorf(n.Pos(), "No metric bound to identifier %q", n.Name)
			return nil, n
		}
		c.emit(bytecode.Instr{bytecode.Mload, n.Symbol.Addr})
		m := n.Symbol.Binding.(*metrics.Metric)
		c.emit(bytecode.Instr{bytecode.Dload, len(m.Keys)})

		if !n.Lvalue {
			t := n.Type()
			if types.IsDimension(t) {
				l := len(t.(*types.TypeOperator).Args)
				t = t.(*types.TypeOperator).Args[l-1]
			}

			if types.Equals(t, types.Float) {
				c.emit(bytecode.Instr{bytecode.Fget, nil})
			} else if types.Equals(t, types.Int) {
				c.emit(bytecode.Instr{bytecode.Iget, nil})
			} else if types.Equals(t, types.String) {
				c.emit(bytecode.Instr{bytecode.Sget, nil})
			} else {
				c.errorf(n.Pos(), "invalid type for get %q in %#v", n.Type(), n)
			}
		}

	case *ast.CaprefNode:
		if n.Symbol == nil || n.Symbol.Binding == nil {
			c.errorf(n.Pos(), "No regular expression bound to capref %q", n.Name)
			return nil, n
		}
		rn := n.Symbol.Binding.(*ast.PatternExpr)
		// rn.index contains the index of the compiled regular expression object
		// in the re slice of the object code
		c.emit(bytecode.Instr{bytecode.Push, rn.Index})
		// n.Symbol.Addr is the capture group offset
		c.emit(bytecode.Instr{bytecode.Capref, n.Symbol.Addr})
		if types.Equals(n.Type(), types.Float) {
			c.emit(bytecode.Instr{bytecode.S2f, nil})
		} else if types.Equals(n.Type(), types.Int) {
			c.emit(bytecode.Instr{bytecode.S2i, nil})
		}

	case *ast.IndexedExpr:
		if args, ok := n.Index.(*ast.ExprList); ok {
			for _, arg := range args.Children {
				_ = ast.Walk(c, arg)
				if types.Equals(arg.Type(), types.Float) {
					c.emit(bytecode.Instr{bytecode.F2s, nil})
				} else if types.Equals(arg.Type(), types.Int) {
					c.emit(bytecode.Instr{bytecode.I2s, nil})
				}
			}
		}
		ast.Walk(c, n.Lhs)
		return nil, n

	case *ast.DecoDefNode:
		// Do nothing, defs are inlined.
		return nil, n

	case *ast.DecoNode:
		// Put the current block on the stack
		c.decos = append(c.decos, n)
		if n.Def == nil {
			c.errorf(n.Pos(), "No definition found for decorator %q", n.Name)
			return nil, n
		}
		// then iterate over the decorator's nodes
		ast.Walk(c, n.Def.Block)
		c.decos = c.decos[:len(c.decos)-1]
		return nil, n

	case *ast.NextNode:
		// Visit the 'next' block on the decorated block stack
		deco := c.decos[len(c.decos)-1]
		ast.Walk(c, deco.Block)
		return nil, n

	case *ast.OtherwiseNode:
		c.emit(bytecode.Instr{Opcode: bytecode.Otherwise})

	case *ast.DelNode:
		if n.Expiry > 0 {
			c.emit(bytecode.Instr{bytecode.Push, n.Expiry})
		}
		ast.Walk(c, n.N)
		// overwrite the dload instruction
		pc := c.pc()
		c.obj.prog[pc].Opcode = bytecode.Del
		if n.Expiry > 0 {
			c.obj.prog[pc].Opcode = bytecode.Expire
		}

	case *ast.BinaryExpr:
		switch n.Op {
		case parser.AND:
			lFalse := c.newLabel()
			lEnd := c.newLabel()
			ast.Walk(c, n.Lhs)
			c.emit(bytecode.Instr{bytecode.Jnm, lFalse})
			ast.Walk(c, n.Rhs)
			c.emit(bytecode.Instr{bytecode.Jnm, lFalse})
			c.emit(bytecode.Instr{bytecode.Push, true})
			c.emit(bytecode.Instr{bytecode.Jmp, lEnd})
			c.setLabel(lFalse)
			c.emit(bytecode.Instr{bytecode.Push, false})
			c.setLabel(lEnd)
			return nil, n

		case parser.OR:
			lTrue := c.newLabel()
			lEnd := c.newLabel()
			ast.Walk(c, n.Lhs)
			c.emit(bytecode.Instr{bytecode.Jm, lTrue})
			ast.Walk(c, n.Rhs)
			c.emit(bytecode.Instr{bytecode.Jm, lTrue})
			c.emit(bytecode.Instr{bytecode.Push, false})
			c.emit(bytecode.Instr{bytecode.Jmp, lEnd})
			c.setLabel(lTrue)
			c.emit(bytecode.Instr{bytecode.Push, true})
			c.setLabel(lEnd)
			return nil, n

		case parser.ADD_ASSIGN:
			if !types.Equals(n.Type(), types.Int) {
				// Double-emit the lhs so that it can be assigned to
				ast.Walk(c, n.Lhs)
			}

		default:
			// Didn't handle it, let normal walk proceed
			return c, n
		}

	}

	return c, node
}

var typedOperators = map[int]map[types.Type]bytecode.Opcode{
	parser.PLUS: {types.Int: bytecode.Iadd,
		types.Float:  bytecode.Fadd,
		types.String: bytecode.Cat},
	parser.MINUS: {types.Int: bytecode.Isub,
		types.Float: bytecode.Fsub},
	parser.MUL: {types.Int: bytecode.Imul,
		types.Float: bytecode.Fmul},
	parser.DIV: {types.Int: bytecode.Idiv,
		types.Float: bytecode.Fdiv},
	parser.MOD: {types.Int: bytecode.Imod,
		types.Float: bytecode.Fmod},
	parser.POW: {types.Int: bytecode.Ipow,
		types.Float: bytecode.Fpow},
	parser.ASSIGN: {types.Int: bytecode.Iset,
		types.Float:  bytecode.Fset,
		types.String: bytecode.Sset},
}

func getOpcodeForType(op int, opT types.Type) (bytecode.Opcode, error) {
	opmap, ok := typedOperators[op]
	if !ok {
		return -1, errors.Errorf("no typed operator for type %v", op)
	}
	for t, opcode := range opmap {
		if types.Equals(t, opT) {
			return opcode, nil
		}
	}
	return -1, errors.Errorf("no opcode for type %s in op %v", opT, op)
}

var builtin = map[string]bytecode.Opcode{
	"getfilename": bytecode.Getfilename,
	"len":         bytecode.Length,
	"settime":     bytecode.Settime,
	"strptime":    bytecode.Strptime,
	"strtol":      bytecode.S2i,
	"timestamp":   bytecode.Timestamp,
	"tolower":     bytecode.Tolower,
}

func (c *codegen) VisitAfter(node ast.Node) ast.Node {
	switch n := node.(type) {
	case *ast.BuiltinNode:
		arglen := 0
		if n.Args != nil {
			arglen = len(n.Args.(*ast.ExprList).Children)
		}
		switch n.Name {
		case "bool":
		// TODO(jaq): Nothing, no support in VM yet.

		case "int", "float", "string":
			// len args should be 1
			if arglen > 1 {
				c.errorf(n.Pos(), "too many arguments to builtin %q: %#v", n.Name, n)
				return n
			}
			if err := c.emitConversion(n.Args.(*ast.ExprList).Children[0].Type(), n.Type()); err != nil {
				c.errorf(n.Pos(), "%s on node %v", err.Error(), n)
				return n
			}

		default:
			c.emit(bytecode.Instr{builtin[n.Name], arglen})
		}
	case *ast.UnaryExpr:
		switch n.Op {
		case parser.INC:
			c.emit(bytecode.Instr{Opcode: bytecode.Inc})
		case parser.DEC:
			c.emit(bytecode.Instr{Opcode: bytecode.Dec})
		case parser.NOT:
			c.emit(bytecode.Instr{Opcode: bytecode.Neg})
		}
	case *ast.BinaryExpr:
		switch n.Op {
		case parser.LT, parser.GT, parser.LE, parser.GE, parser.EQ, parser.NE:
			lFail := c.newLabel()
			lEnd := c.newLabel()
			var cmpArg int
			var jumpOp bytecode.Opcode
			switch n.Op {
			case parser.LT:
				cmpArg = -1
				jumpOp = bytecode.Jnm
			case parser.GT:
				cmpArg = 1
				jumpOp = bytecode.Jnm
			case parser.LE:
				cmpArg = 1
				jumpOp = bytecode.Jm
			case parser.GE:
				cmpArg = -1
				jumpOp = bytecode.Jm
			case parser.EQ:
				cmpArg = 0
				jumpOp = bytecode.Jnm
			case parser.NE:
				cmpArg = 0
				jumpOp = bytecode.Jm
			}
			cmpOp := bytecode.Cmp
			if types.Equals(n.Lhs.Type(), n.Rhs.Type()) {
				switch n.Lhs.Type() {
				case types.Float:
					cmpOp = bytecode.Fcmp
				case types.Int:
					cmpOp = bytecode.Icmp
				case types.String:
					cmpOp = bytecode.Scmp
				default:
					cmpOp = bytecode.Cmp
				}
			}
			c.emit(bytecode.Instr{cmpOp, cmpArg})
			c.emit(bytecode.Instr{jumpOp, lFail})
			c.emit(bytecode.Instr{bytecode.Push, true})
			c.emit(bytecode.Instr{bytecode.Jmp, lEnd})
			c.setLabel(lFail)
			c.emit(bytecode.Instr{bytecode.Push, false})
			c.setLabel(lEnd)
		case parser.ADD_ASSIGN:
			// When operand is not nil, inc pops the delta from the stack.
			switch {
			case types.Equals(n.Type(), types.Int):
				c.emit(bytecode.Instr{bytecode.Inc, 0})
			case types.Equals(n.Type(), types.Float), types.Equals(n.Type(), types.String):
				// Already walked the lhs and rhs of this expression
				opcode, err := getOpcodeForType(parser.PLUS, n.Type())
				if err != nil {
					c.errorf(n.Pos(), "%s", err)
					return n
				}
				c.emit(bytecode.Instr{Opcode: opcode})
				// And a second lhs
				opcode, err = getOpcodeForType(parser.ASSIGN, n.Type())
				if err != nil {
					c.errorf(n.Pos(), "%s", err)
					return n
				}
				c.emit(bytecode.Instr{Opcode: opcode})
			default:
				c.errorf(n.Pos(), "invalid type for add-assignment: %v", n.Type())
				return n
			}
		case parser.PLUS, parser.MINUS, parser.MUL, parser.DIV, parser.MOD, parser.POW, parser.ASSIGN:
			opcode, err := getOpcodeForType(n.Op, n.Type())
			if err != nil {
				c.errorf(n.Pos(), "%s", err)
				return n
			}
			c.emit(bytecode.Instr{Opcode: opcode})
		case parser.BITAND:
			c.emit(bytecode.Instr{Opcode: bytecode.And})
		case parser.BITOR:
			c.emit(bytecode.Instr{Opcode: bytecode.Or})
		case parser.XOR:
			c.emit(bytecode.Instr{Opcode: bytecode.Xor})
		case parser.SHL:
			c.emit(bytecode.Instr{Opcode: bytecode.Shl})
		case parser.SHR:
			c.emit(bytecode.Instr{Opcode: bytecode.Shr})

		case parser.MATCH:
			// Cross fingers that last branch was a patternExprNode
			c.obj.prog[c.pc()].Opcode = bytecode.Smatch

		case parser.NOT_MATCH:
			// Cross fingers that last branch was a patternExprNode
			c.obj.prog[c.pc()].Opcode = bytecode.Smatch
			c.emit(bytecode.Instr{Opcode: bytecode.Not})

		case parser.CONCAT:
			// skip

		default:
			c.errorf(n.Pos(), "unexpected op %v", n.Op)
		}

	case *ast.ConvNode:
		if err := c.emitConversion(n.N.Type(), n.Type()); err != nil {
			c.errorf(n.Pos(), "internal error: %s on node %v", err.Error(), n)
			return n
		}
	}
	return node
}

func (c *codegen) emitConversion(inType, outType types.Type) error {
	glog.V(2).Infof("Conversion: %q to %q", inType, outType)
	if types.Equals(types.Int, inType) && types.Equals(types.Float, outType) {
		c.emit(bytecode.Instr{Opcode: bytecode.I2f})
	} else if types.Equals(types.String, inType) && types.Equals(types.Float, outType) {
		c.emit(bytecode.Instr{Opcode: bytecode.S2f})
	} else if types.Equals(types.String, inType) && types.Equals(types.Int, outType) {
		c.emit(bytecode.Instr{Opcode: bytecode.S2i})
	} else if types.Equals(types.Float, inType) && types.Equals(types.String, outType) {
		c.emit(bytecode.Instr{Opcode: bytecode.F2s})
	} else if types.Equals(types.Int, inType) && types.Equals(types.String, outType) {
		c.emit(bytecode.Instr{Opcode: bytecode.I2s})
	} else if types.Equals(types.Pattern, inType) && types.Equals(types.Bool, outType) {
		// nothing, pattern is implicit bool
	} else if types.Equals(inType, outType) {
		// Nothing; no-op.
	} else {
		return errors.Errorf("can't convert %q to %q", inType, outType)
	}
	return nil
}

func (c *codegen) writeJumps() {
	for j, i := range c.obj.prog {
		switch i.Opcode {
		case bytecode.Jmp, bytecode.Jm, bytecode.Jnm:
			index := i.Operand.(int)
			if index > len(c.l) {
				c.errorf(nil, "no jump at label %v, table is %v", i.Operand, c.l)
				continue
			}
			offset := c.l[index]
			if offset < 0 {
				c.errorf(nil, "offset for label %v is negative, table is %v", i.Operand, c.l)
				continue
			}
			c.obj.prog[j].Operand = c.l[index]
		}
	}
}
