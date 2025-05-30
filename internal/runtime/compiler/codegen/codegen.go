// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package codegen

import (
	"fmt"
	"math"
	"regexp"
	"time"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/runtime/code"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/errors"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/symbol"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/types"
)

// codegen represents a code generator.
type codegen struct {
	name string // Name of the program.

	errors errors.ErrorList // Any compile errors detected are accumulated here.
	obj    code.Object      // The object to return, if successful.

	l     []int           // Label table for recording jump destinations.
	decos []*ast.DecoStmt // Decorator stack to unwind when entering decorated blocks.
}

// CodeGen is the function that compiles the program to bytecode and data.
func CodeGen(name string, n ast.Node) (*code.Object, error) {
	c := &codegen{name: name}
	_ = ast.Walk(c, n)
	c.writeJumps()
	if len(c.errors) > 0 {
		return nil, &c.errors
	}
	return &c.obj, nil
}

func (c *codegen) errorf(pos *position.Position, format string, args ...interface{}) {
	e := "Internal compiler error, aborting compilation: " + fmt.Sprintf(format, args...)
	c.errors.Add(pos, e)
}

func (c *codegen) emit(n ast.Node, opcode code.Opcode, operand interface{}) {
	glog.V(2).Infof("emitting `%s %v' from line %d node %#v\n", opcode, operand, n.Pos().Line, n)
	c.obj.Program = append(c.obj.Program, code.Instr{opcode, operand, n.Pos().Line})
}

// newLabel creates a new label to jump to.
func (c *codegen) newLabel() (l int) {
	l = len(c.l)
	c.l = append(c.l, -1)
	return
}

// setLabel points a label to the next instruction.
func (c *codegen) setLabel(l int) {
	c.l[l] = c.pc() + 1
}

// pc returns the program offset of the last instruction.
func (c *codegen) pc() int {
	return len(c.obj.Program) - 1
}

func (c *codegen) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	switch n := node.(type) {

	case *ast.VarDecl:
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
			t = t.(*types.Operator).Args[len(t.(*types.Operator).Args)-1]
		}
		var dtyp metrics.Type
		switch {
		case types.Equals(types.Float, t):
			dtyp = metrics.Float
		case types.Equals(types.String, t):
			dtyp = metrics.String
		case types.Equals(types.Buckets, t):
			dtyp = metrics.Buckets
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
			// Calling GetDatum here causes the storage to be allocated.
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
				c.errorf(n.Pos(), "Can't initialize to zero a %#v", n)
				return nil, n
			}
		}

		if n.Kind == metrics.Histogram {
			if len(n.Buckets) < 2 {
				c.errorf(n.Pos(), "a histogram need at least two boundaries")
				return nil, n
			}

			if n.Buckets[0] > 0 {
				m.Buckets = append(m.Buckets, datum.Range{0, n.Buckets[0]})
			}
			lo := n.Buckets[0]
			for _, hi := range n.Buckets[1:] {
				if hi <= lo {
					c.errorf(n.Pos(), "buckets boundaries must be sorted")
					return nil, n
				}
				m.Buckets = append(m.Buckets, datum.Range{lo, hi})
				lo = hi
			}
			m.Buckets = append(m.Buckets, datum.Range{lo, math.Inf(+1)})

			if len(n.Keys) == 0 {
				// Calling GetDatum here causes the storage to be allocated.
				_, err := m.GetDatum()
				if err != nil {
					c.errorf(n.Pos(), "%s", err)
					return nil, n
				}
			}
		}

		m.Hidden = n.Hidden
		// int is int64 only on 64bit platforms.  To be fair MaxInt is a
		// ridiculously excessive size for this anyway, you're going to use 2GiB
		// x sizeof(datum) in a single metric.
		if n.Limit > math.MaxInt {
			c.errorf(n.Pos(), "limit %d too large; max %d", n.Limit, math.MaxInt)
			return nil, n
		}
		m.Limit = int(n.Limit)

		n.Symbol.Binding = m
		n.Symbol.Addr = len(c.obj.Metrics)
		c.obj.Metrics = append(c.obj.Metrics, m)
		return nil, n

	case *ast.CondStmt:
		lElse := c.newLabel()
		lEnd := c.newLabel()
		if n.Cond != nil {
			n.Cond = ast.Walk(c, n.Cond)
			c.emit(n, code.Jnm, lElse)
		}
		// Set matched flag false for children.
		c.emit(n, code.Setmatched, false)
		n.Truth = ast.Walk(c, n.Truth)
		// Re-set matched flag to true for rest of current block.
		c.emit(n, code.Setmatched, true)
		if n.Else != nil {
			c.emit(n, code.Jmp, lEnd)
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
		c.obj.Regexps = append(c.obj.Regexps, re)
		// Store the location of this regular expression in the PatternExpr
		n.Index = len(c.obj.Regexps) - 1
		return nil, n

	case *ast.PatternFragment:
		// Skip, const pattern fragments are concatenated into PatternExpr storage, not executable.
		return nil, n

	case *ast.StringLit:
		c.obj.Strings = append(c.obj.Strings, n.Text)
		c.emit(n, code.Str, len(c.obj.Strings)-1)

	case *ast.IntLit:
		c.emit(n, code.Push, n.I)

	case *ast.FloatLit:
		c.emit(n, code.Push, n.F)

	case *ast.StopStmt:
		c.emit(n, code.Stop, nil)

	case *ast.IDTerm:
		if n.Symbol == nil || n.Symbol.Kind != symbol.VarSymbol {
			break
		}
		if n.Symbol.Binding == nil {
			c.errorf(n.Pos(), "No metric bound to identifier %q", n.Name)
			return nil, n
		}
		c.emit(n, code.Mload, n.Symbol.Addr)
		m := n.Symbol.Binding.(*metrics.Metric)
		c.emit(n, code.Dload, len(m.Keys))

		if !n.Lvalue {
			t := n.Type()
			if types.IsDimension(t) {
				l := len(t.(*types.Operator).Args)
				t = t.(*types.Operator).Args[l-1]
			}

			switch {
			case types.Equals(t, types.Float):
				c.emit(n, code.Fget, nil)
			case types.Equals(t, types.Int):
				c.emit(n, code.Iget, nil)
			case types.Equals(t, types.String):
				c.emit(n, code.Sget, nil)
			default:
				c.errorf(n.Pos(), "invalid type for get %q in %#v", n.Type(), n)
				return nil, n
			}
		}

	case *ast.CaprefTerm:
		if n.Symbol == nil || n.Symbol.Binding == nil {
			c.errorf(n.Pos(), "No regular expression bound to capref %q", n.Name)
			return nil, n
		}
		rn := n.Symbol.Binding.(*ast.PatternExpr)
		// rn.index contains the index of the compiled regular expression object
		// in the re slice of the object code
		c.emit(n, code.Push, rn.Index)
		// n.Symbol.Addr is the capture group offset
		c.emit(n, code.Capref, n.Symbol.Addr)
		if types.Equals(n.Type(), types.Float) {
			c.emit(n, code.S2f, nil)
		} else if types.Equals(n.Type(), types.Int) {
			c.emit(n, code.S2i, nil)
		}

	case *ast.IndexedExpr:
		if args, ok := n.Index.(*ast.ExprList); ok {
			for _, arg := range args.Children {
				_ = ast.Walk(c, arg)
				if types.Equals(arg.Type(), types.Float) {
					c.emit(n, code.F2s, nil)
				} else if types.Equals(arg.Type(), types.Int) {
					c.emit(n, code.I2s, nil)
				}
			}
		}
		ast.Walk(c, n.LHS)
		return nil, n

	case *ast.DecoDecl:
		// Do nothing, defs are inlined.
		return nil, n

	case *ast.DecoStmt:
		// Put the current block on the stack
		decoLen := len(c.decos)
		c.decos = append(c.decos, n)
		if n.Decl == nil {
			c.errorf(n.Pos(), "No definition found for decorator %q", n.Name)
			return nil, n
		}
		// then iterate over the decorator's nodes
		ast.Walk(c, n.Decl.Block)
		if len(c.decos) > decoLen {
			glog.V(1).Info("Too many blocks on stack, was there no `next' in the last one?")
		}
		return nil, n

	case *ast.NextStmt:
		// Visit the 'next' block on the decorated block stack
		top := len(c.decos) - 1
		deco := c.decos[top]
		c.decos = c.decos[:top]
		ast.Walk(c, deco.Block)
		return nil, n

	case *ast.OtherwiseStmt:
		c.emit(n, code.Otherwise, nil)

	case *ast.DelStmt:
		if n.Expiry > 0 {
			c.emit(n, code.Push, n.Expiry)
		}
		ast.Walk(c, n.N)
		// overwrite the dload instruction
		pc := c.pc()
		c.obj.Program[pc].Opcode = code.Del
		if n.Expiry > 0 {
			c.obj.Program[pc].Opcode = code.Expire
		}

	case *ast.BinaryExpr:
		switch n.Op {
		case parser.AND:
			lFalse := c.newLabel()
			lEnd := c.newLabel()
			ast.Walk(c, n.LHS)
			c.emit(n, code.Jnm, lFalse)
			ast.Walk(c, n.RHS)
			c.emit(n, code.Jnm, lFalse)
			c.emit(n, code.Push, true)
			c.emit(n, code.Jmp, lEnd)
			c.setLabel(lFalse)
			c.emit(n, code.Push, false)
			c.setLabel(lEnd)
			return nil, n

		case parser.OR:
			lTrue := c.newLabel()
			lEnd := c.newLabel()
			ast.Walk(c, n.LHS)
			c.emit(n, code.Jm, lTrue)
			ast.Walk(c, n.RHS)
			c.emit(n, code.Jm, lTrue)
			c.emit(n, code.Push, false)
			c.emit(n, code.Jmp, lEnd)
			c.setLabel(lTrue)
			c.emit(n, code.Push, true)
			c.setLabel(lEnd)
			return nil, n

		case parser.ADD_ASSIGN:
			if !types.Equals(n.Type(), types.Int) {
				// Double-emit the lhs so that it can be assigned to
				ast.Walk(c, n.LHS)
			}

		default:
			// Didn't handle it, let normal walk proceed
			return c, n
		}
	}

	return c, node
}

var typedOperators = map[int]map[types.Type]code.Opcode{
	parser.PLUS: {
		types.Int:     code.Iadd,
		types.Float:   code.Fadd,
		types.String:  code.Cat,
		types.Pattern: code.Cat,
	},
	parser.MINUS: {
		types.Int:   code.Isub,
		types.Float: code.Fsub,
	},
	parser.MUL: {
		types.Int:   code.Imul,
		types.Float: code.Fmul,
	},
	parser.DIV: {
		types.Int:   code.Idiv,
		types.Float: code.Fdiv,
	},
	parser.MOD: {
		types.Int:   code.Imod,
		types.Float: code.Fmod,
	},
	parser.POW: {
		types.Int:   code.Ipow,
		types.Float: code.Fpow,
	},
	parser.ASSIGN: {
		types.Int:    code.Iset,
		types.Float:  code.Fset,
		types.String: code.Sset,
	},
}

func getOpcodeForType(op int, opT types.Type) (code.Opcode, error) {
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

var builtin = map[string]code.Opcode{
	"getfilename": code.Getfilename,
	"len":         code.Length,
	"settime":     code.Settime,
	"strptime":    code.Strptime,
	"strtol":      code.S2i,
	"subst":       code.Subst,
	"timestamp":   code.Timestamp,
	"tolower":     code.Tolower,
}

func (c *codegen) VisitAfter(node ast.Node) ast.Node {
	switch n := node.(type) {
	case *ast.BuiltinExpr:
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
			if err := c.emitConversion(n, n.Args.(*ast.ExprList).Children[0].Type(), n.Type()); err != nil {
				c.errorf(n.Pos(), "%s on node %v", err.Error(), n)
				return n
			}
		case "subst":
			if types.Equals(n.Args.(*ast.ExprList).Children[0].Type(), types.Pattern) {
				index := n.Args.(*ast.ExprList).Children[0].(*ast.PatternExpr).Index
				c.emit(n, code.Push, index)
				c.emit(n, code.Rsubst, arglen)
			} else {
				c.emit(n, code.Subst, arglen)
			}

		default:
			c.emit(n, builtin[n.Name], arglen)
		}
	case *ast.UnaryExpr:
		switch n.Op {
		case parser.INC:
			c.emit(n, code.Inc, nil)
		case parser.DEC:
			c.emit(n, code.Dec, nil)
		case parser.NOT:
			c.emit(n, code.Neg, nil)
		case parser.MATCH:
			index := n.Expr.(*ast.PatternExpr).Index
			c.emit(n, code.Match, index)
		}
	case *ast.BinaryExpr:
		switch n.Op {
		case parser.LT, parser.GT, parser.LE, parser.GE, parser.EQ, parser.NE:
			lFail := c.newLabel()
			lEnd := c.newLabel()
			var cmpArg int
			var jumpOp code.Opcode
			switch n.Op {
			case parser.LT:
				cmpArg = -1
				jumpOp = code.Jnm
			case parser.GT:
				cmpArg = 1
				jumpOp = code.Jnm
			case parser.LE:
				cmpArg = 1
				jumpOp = code.Jm
			case parser.GE:
				cmpArg = -1
				jumpOp = code.Jm
			case parser.EQ:
				cmpArg = 0
				jumpOp = code.Jnm
			case parser.NE:
				cmpArg = 0
				jumpOp = code.Jm
			}
			cmpOp := code.Cmp
			if types.Equals(n.LHS.Type(), n.RHS.Type()) {
				switch n.LHS.Type() {
				case types.Float:
					cmpOp = code.Fcmp
				case types.Int:
					cmpOp = code.Icmp
				case types.String:
					cmpOp = code.Scmp
				default:
					cmpOp = code.Cmp
				}
			}
			c.emit(n, cmpOp, cmpArg)
			c.emit(n, jumpOp, lFail)
			c.emit(n, code.Push, true)
			c.emit(n, code.Jmp, lEnd)
			c.setLabel(lFail)
			c.emit(n, code.Push, false)
			c.setLabel(lEnd)
		case parser.ADD_ASSIGN:
			// When operand is not nil, inc pops the delta from the stack.
			switch {
			case types.Equals(n.Type(), types.Int):
				c.emit(n, code.Inc, 0)
			case types.Equals(n.Type(), types.Float), types.Equals(n.Type(), types.String):
				// Already walked the lhs and rhs of this expression
				opcode, err := getOpcodeForType(parser.PLUS, n.Type())
				if err != nil {
					c.errorf(n.Pos(), "%s", err)
					return n
				}
				c.emit(n, opcode, nil)
				// And a second lhs
				opcode, err = getOpcodeForType(parser.ASSIGN, n.Type())
				if err != nil {
					c.errorf(n.Pos(), "%s", err)
					return n
				}
				c.emit(n, opcode, nil)
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
			c.emit(n, opcode, nil)
		case parser.BITAND:
			c.emit(n, code.And, nil)
		case parser.BITOR:
			c.emit(n, code.Or, nil)
		case parser.XOR:
			c.emit(n, code.Xor, nil)
		case parser.SHL:
			c.emit(n, code.Shl, nil)
		case parser.SHR:
			c.emit(n, code.Shr, nil)

		case parser.MATCH, parser.NOT_MATCH:
			switch v := n.RHS.(type) {
			case *ast.PatternExpr:
				index := v.Index
				c.emit(n, code.Smatch, index)
			default:
				c.errorf(n.Pos(), "unexpected rhs expression for match %#v", n.RHS)
				return n
			}

			if n.Op == parser.NOT_MATCH {
				c.emit(n, code.Not, nil)
			}

		default:
			c.errorf(n.Pos(), "unexpected op %v", n.Op)
		}

	case *ast.ConvExpr:
		if err := c.emitConversion(n, n.N.Type(), n.Type()); err != nil {
			c.errorf(n.Pos(), "internal error: %s on node %v", err.Error(), n)
			return n
		}
	}
	return node
}

func (c *codegen) emitConversion(n ast.Node, inType, outType types.Type) error {
	glog.V(2).Infof("Conversion: %q to %q", inType, outType)
	switch {
	case types.Equals(types.Int, inType) && types.Equals(types.Float, outType):
		c.emit(n, code.I2f, nil)
	case types.Equals(types.String, inType) && types.Equals(types.Float, outType):
		c.emit(n, code.S2f, nil)
	case types.Equals(types.String, inType) && types.Equals(types.Int, outType):
		c.emit(n, code.S2i, nil)
	case types.Equals(types.Float, inType) && types.Equals(types.String, outType):
		c.emit(n, code.F2s, nil)
	case types.Equals(types.Int, inType) && types.Equals(types.String, outType):
		c.emit(n, code.I2s, nil)
	case types.Equals(types.Pattern, inType) && types.Equals(types.Bool, outType):
		// nothing, pattern is implicit bool
	case types.Equals(inType, outType):
		// Nothing; no-op.
	default:
		return errors.Errorf("can't convert %q to %q", inType, outType)
	}
	return nil
}

func (c *codegen) writeJumps() {
	for j, i := range c.obj.Program {
		switch i.Opcode {
		case code.Jmp, code.Jm, code.Jnm:
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
			c.obj.Program[j].Operand = c.l[index]
		}
	}
}
