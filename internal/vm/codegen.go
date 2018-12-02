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
	"github.com/google/mtail/internal/vm/errors"
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

func (c *codegen) emit(i instr) {
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
			c.emit(instr{jnm, lElse})
		}
		// Set matched flag false for children.
		c.emit(instr{setmatched, false})
		n.Truth = ast.Walk(c, n.Truth)
		// Re-set matched flag to true for rest of current block.
		c.emit(instr{setmatched, true})
		if n.Else != nil {
			c.emit(instr{jmp, lEnd})
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
		c.emit(instr{match, n.Index})

	case *ast.StringConst:
		c.obj.str = append(c.obj.str, n.Text)
		c.emit(instr{str, len(c.obj.str) - 1})

	case *ast.IntConst:
		c.emit(instr{push, n.I})

	case *ast.FloatConst:
		c.emit(instr{push, n.F})

	case *ast.StopNode:
		c.emit(instr{stop, nil})

	case *ast.Id:
		if n.Symbol == nil || n.Symbol.Kind != symtab.VarSymbol {
			break
		}
		if n.Symbol.Binding == nil {
			c.errorf(n.Pos(), "No metric bound to identifier %q", n.Name)
			return nil, n
		}
		c.emit(instr{mload, n.Symbol.Addr})
		m := n.Symbol.Binding.(*metrics.Metric)
		c.emit(instr{dload, len(m.Keys)})

		if !n.Lvalue {
			t := n.Type()
			if types.IsDimension(t) {
				l := len(t.(*types.TypeOperator).Args)
				t = t.(*types.TypeOperator).Args[l-1]
			}

			if types.Equals(t, types.Float) {
				c.emit(instr{fget, nil})
			} else if types.Equals(t, types.Int) {
				c.emit(instr{iget, nil})
			} else if types.Equals(t, types.String) {
				c.emit(instr{sget, nil})
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
		c.emit(instr{push, rn.Index})
		// n.Symbol.Addr is the capture group offset
		c.emit(instr{capref, n.Symbol.Addr})
		if types.Equals(n.Type(), types.Float) {
			c.emit(instr{s2f, nil})
		} else if types.Equals(n.Type(), types.Int) {
			c.emit(instr{s2i, nil})
		}

	case *ast.IndexedExpr:
		if args, ok := n.Index.(*ast.ExprList); ok {
			for _, arg := range args.Children {
				_ = ast.Walk(c, arg)
				if types.Equals(arg.Type(), types.Float) {
					c.emit(instr{f2s, nil})
				} else if types.Equals(arg.Type(), types.Int) {
					c.emit(instr{i2s, nil})
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
		c.emit(instr{op: otherwise})

	case *ast.DelNode:
		if n.Expiry > 0 {
			c.emit(instr{push, n.Expiry})
		}
		ast.Walk(c, n.N)
		// overwrite the dload instruction
		pc := c.pc()
		c.obj.prog[pc].op = del
		if n.Expiry > 0 {
			c.obj.prog[pc].op = expire
		}

	case *ast.BinaryExpr:
		switch n.Op {
		case AND:
			lFalse := c.newLabel()
			lEnd := c.newLabel()
			ast.Walk(c, n.Lhs)
			c.emit(instr{jnm, lFalse})
			ast.Walk(c, n.Rhs)
			c.emit(instr{jnm, lFalse})
			c.emit(instr{push, true})
			c.emit(instr{jmp, lEnd})
			c.setLabel(lFalse)
			c.emit(instr{push, false})
			c.setLabel(lEnd)
			return nil, n

		case OR:
			lTrue := c.newLabel()
			lEnd := c.newLabel()
			ast.Walk(c, n.Lhs)
			c.emit(instr{jm, lTrue})
			ast.Walk(c, n.Rhs)
			c.emit(instr{jm, lTrue})
			c.emit(instr{push, false})
			c.emit(instr{jmp, lEnd})
			c.setLabel(lTrue)
			c.emit(instr{push, true})
			c.setLabel(lEnd)
			return nil, n

		case ADD_ASSIGN:
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

var typedOperators = map[int]map[types.Type]opcode{
	PLUS: {types.Int: iadd,
		types.Float:  fadd,
		types.String: cat},
	MINUS: {types.Int: isub,
		types.Float: fsub},
	MUL: {types.Int: imul,
		types.Float: fmul},
	DIV: {types.Int: idiv,
		types.Float: fdiv},
	MOD: {types.Int: imod,
		types.Float: fmod},
	POW: {types.Int: ipow,
		types.Float: fpow},
	ASSIGN: {types.Int: iset,
		types.Float:  fset,
		types.String: sset},
}

func getOpcodeForType(op int, opT types.Type) (opcode, error) {
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
			c.emit(instr{builtin[n.Name], arglen})
		}
	case *ast.UnaryExpr:
		switch n.Op {
		case INC:
			c.emit(instr{op: inc})
		case DEC:
			c.emit(instr{op: dec})
		case NOT:
			c.emit(instr{op: neg})
		}
	case *ast.BinaryExpr:
		switch n.Op {
		case LT, GT, LE, GE, EQ, NE:
			lFail := c.newLabel()
			lEnd := c.newLabel()
			var cmpArg int
			var jumpOp opcode
			switch n.Op {
			case LT:
				cmpArg = -1
				jumpOp = jnm
			case GT:
				cmpArg = 1
				jumpOp = jnm
			case LE:
				cmpArg = 1
				jumpOp = jm
			case GE:
				cmpArg = -1
				jumpOp = jm
			case EQ:
				cmpArg = 0
				jumpOp = jnm
			case NE:
				cmpArg = 0
				jumpOp = jm
			}
			cmpOp := cmp
			if types.Equals(n.Lhs.Type(), n.Rhs.Type()) {
				switch n.Lhs.Type() {
				case types.Float:
					cmpOp = fcmp
				case types.Int:
					cmpOp = icmp
				case types.String:
					cmpOp = scmp
				default:
					cmpOp = cmp
				}
			}
			c.emit(instr{cmpOp, cmpArg})
			c.emit(instr{jumpOp, lFail})
			c.emit(instr{push, true})
			c.emit(instr{jmp, lEnd})
			c.setLabel(lFail)
			c.emit(instr{push, false})
			c.setLabel(lEnd)
		case ADD_ASSIGN:
			// When operand is not nil, inc pops the delta from the stack.
			switch {
			case types.Equals(n.Type(), types.Int):
				c.emit(instr{inc, 0})
			case types.Equals(n.Type(), types.Float), types.Equals(n.Type(), types.String):
				// Already walked the lhs and rhs of this expression
				opcode, err := getOpcodeForType(PLUS, n.Type())
				if err != nil {
					c.errorf(n.Pos(), "%s", err)
					return n
				}
				c.emit(instr{op: opcode})
				// And a second lhs
				opcode, err = getOpcodeForType(ASSIGN, n.Type())
				if err != nil {
					c.errorf(n.Pos(), "%s", err)
					return n
				}
				c.emit(instr{op: opcode})
			default:
				c.errorf(n.Pos(), "invalid type for add-assignment: %v", n.Type())
				return n
			}
		case PLUS, MINUS, MUL, DIV, MOD, POW, ASSIGN:
			opcode, err := getOpcodeForType(n.Op, n.Type())
			if err != nil {
				c.errorf(n.Pos(), "%s", err)
				return n
			}
			c.emit(instr{op: opcode})
		case BITAND:
			c.emit(instr{op: and})
		case BITOR:
			c.emit(instr{op: or})
		case XOR:
			c.emit(instr{op: xor})
		case SHL:
			c.emit(instr{op: shl})
		case SHR:
			c.emit(instr{op: shr})

		case MATCH:
			// Cross fingers that last branch was a patternExprNode
			c.obj.prog[c.pc()].op = smatch

		case NOT_MATCH:
			// Cross fingers that last branch was a patternExprNode
			c.obj.prog[c.pc()].op = smatch
			c.emit(instr{op: not})

		case CONCAT:
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
		c.emit(instr{op: i2f})
	} else if types.Equals(types.String, inType) && types.Equals(types.Float, outType) {
		c.emit(instr{op: s2f})
	} else if types.Equals(types.String, inType) && types.Equals(types.Int, outType) {
		c.emit(instr{op: s2i})
	} else if types.Equals(types.Float, inType) && types.Equals(types.String, outType) {
		c.emit(instr{op: f2s})
	} else if types.Equals(types.Int, inType) && types.Equals(types.String, outType) {
		c.emit(instr{op: i2s})
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
		switch i.op {
		case jmp, jm, jnm:
			index := i.opnd.(int)
			if index > len(c.l) {
				c.errorf(nil, "no jump at label %v, table is %v", i.opnd, c.l)
				continue
			}
			offset := c.l[index]
			if offset < 0 {
				c.errorf(nil, "offset for label %v is negative, table is %v", i.opnd, c.l)
				continue
			}
			c.obj.prog[j].opnd = c.l[index]
		}
	}
}
