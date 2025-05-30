// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package opt_test

import (
	"math"
	"math/rand"
	"reflect"
	"strings"
	"testing"
	"testing/quick"

	"github.com/google/go-cmp/cmp"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/opt"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
	"github.com/jaqx0r/mtail/internal/testutil"
)

var optimiserTests = []struct {
	name string
	ast  ast.Node
	want ast.Node
}{
	{
		"int add",
		&ast.BinaryExpr{
			LHS: &ast.IntLit{I: 1},
			RHS: &ast.IntLit{I: 2},
			Op:  parser.PLUS,
		},
		&ast.IntLit{I: 3},
	},
	{
		"float mul",
		&ast.BinaryExpr{
			LHS: &ast.FloatLit{F: 2},
			RHS: &ast.FloatLit{F: 3},
			Op:  parser.MUL,
		},
		&ast.FloatLit{F: 6},
	},
	{
		"int float pow",
		&ast.BinaryExpr{
			LHS: &ast.IntLit{I: 2},
			RHS: &ast.FloatLit{F: 3},
			Op:  parser.POW,
		},
		&ast.FloatLit{F: 8},
	},
	{
		"float int mod",
		&ast.BinaryExpr{
			LHS: &ast.FloatLit{F: 3},
			RHS: &ast.IntLit{I: 2},
			Op:  parser.MOD,
		},
		&ast.FloatLit{F: 1},
	},
	{
		"nested ops",
		&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.IntLit{I: 2},
				RHS: &ast.IntLit{I: 4},
				Op:  parser.POW,
			},
			RHS: &ast.IntLit{I: 1},
			Op:  parser.MINUS,
		},
		&ast.IntLit{I: 15},
	},
}

func TestOptimiser(t *testing.T) {
	for _, tc := range optimiserTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			got, err := opt.Optimise(tc.ast)
			testutil.FatalIfErr(t, err)
			testutil.ExpectNoDiff(t, tc.want, got)
		})
	}
}

var optimiserErrorTests = []struct {
	name string
	ast  ast.Node
	want []string
}{
	{
		"integer divide by zero",
		&ast.BinaryExpr{
			LHS: &ast.IntLit{I: 4},
			RHS: &ast.IntLit{I: 0},
			Op:  parser.DIV,
		},
		[]string{":1:1: divide by zero"},
	},
	{
		"float divide by zero",
		&ast.BinaryExpr{
			LHS: &ast.FloatLit{F: 4},
			RHS: &ast.FloatLit{F: 0},
			Op:  parser.DIV,
		},
		[]string{":1:1: divide by zero"},
	},
	{
		"integer mod by zero",
		&ast.BinaryExpr{
			LHS: &ast.IntLit{I: 4},
			RHS: &ast.IntLit{I: 0},
			Op:  parser.MOD,
		},
		[]string{":1:1: mod by zero"},
	},
	{
		"float mod by zero",
		&ast.BinaryExpr{
			LHS: &ast.FloatLit{F: 4},
			RHS: &ast.FloatLit{F: 0},
			Op:  parser.MOD,
		},
		[]string{":1:1: mod by zero"},
	},
}

func TestOptimiserErrors(t *testing.T) {
	for _, tc := range optimiserErrorTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			_, err := opt.Optimise(tc.ast)
			testutil.ExpectNoDiff(t, tc.want, strings.Split(err.Error(), "\n"))
		})
	}
}

var commOps = map[int]string{parser.PLUS: "add", parser.MUL: "mul"}

func TestConstFoldQuickIntComm(t *testing.T) {
	for op, name := range commOps {
		op := op
		t.Run(name, func(t *testing.T) {
			if err := quick.Check(func(x, y int32) bool {
				a, aErr := opt.Optimise(&ast.BinaryExpr{
					LHS: &ast.IntLit{I: int64(x)},
					RHS: &ast.IntLit{I: int64(y)},
					Op:  op,
				})
				if aErr != nil {
					t.Fatal(aErr)
				}
				b, bErr := opt.Optimise(&ast.BinaryExpr{
					LHS: &ast.IntLit{I: int64(y)},
					RHS: &ast.IntLit{I: int64(x)},
					Op:  op,
				})
				if bErr != nil {
					t.Fatal(bErr)
				}
				return cmp.Equal(a, b)
			}, nil); err != nil {
				t.Error(err)
			}
		})
	}
}

func TestConstFoldQuickFloatComm(t *testing.T) {
	for op, name := range commOps {
		op := op
		t.Run(name, func(t *testing.T) {
			if err := quick.Check(func(x, y float32) bool {
				a, aErr := opt.Optimise(&ast.BinaryExpr{
					LHS: &ast.FloatLit{F: float64(x)},
					RHS: &ast.FloatLit{F: float64(y)},
					Op:  op,
				})
				if aErr != nil {
					t.Fatal(aErr)
				}
				b, bErr := opt.Optimise(&ast.BinaryExpr{
					LHS: &ast.FloatLit{F: float64(y)},
					RHS: &ast.FloatLit{F: float64(x)},
					Op:  op,
				})
				if bErr != nil {
					t.Fatal(bErr)
				}
				return cmp.Equal(a, b)
			}, nil); err != nil {
				t.Error(err)
			}
		})
	}
}

func TestConstFoldQuickMixedComm(t *testing.T) {
	for op, name := range commOps {
		op := op
		t.Run(name, func(t *testing.T) {
			if err := quick.Check(func(x int32, y float32) bool {
				a, aErr := opt.Optimise(&ast.BinaryExpr{
					LHS: &ast.IntLit{I: int64(x)},
					RHS: &ast.FloatLit{F: float64(y)},
					Op:  op,
				})
				if aErr != nil {
					t.Fatal(aErr)
				}
				b, bErr := opt.Optimise(&ast.BinaryExpr{
					LHS: &ast.FloatLit{F: float64(y)},
					RHS: &ast.IntLit{I: int64(x)},
					Op:  op,
				})
				if bErr != nil {
					t.Fatal(bErr)
				}
				return cmp.Equal(a, b)
			}, nil); err != nil {
				t.Error(err)
			}
		})
	}
}

func TestConstFoldQuickIntAddSub(t *testing.T) {
	if err := quick.Check(func(x, y int32) bool {
		a, aErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.IntLit{I: int64(x)},
			RHS: &ast.IntLit{I: int64(y)},
			Op:  parser.MINUS,
		})
		if aErr != nil {
			t.Fatal(aErr)
		}
		b, bErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.IntLit{I: 0},
				RHS: &ast.IntLit{I: int64(y)},
				Op:  parser.MINUS,
			},
			RHS: &ast.IntLit{I: int64(x)},
			Op:  parser.PLUS,
		})
		if bErr != nil {
			t.Fatal(bErr)
		}
		return cmp.Equal(a, b)
	}, nil); err != nil {
		t.Error(err)
	}
}

func TestConstFoldQuickFloatAddSub(t *testing.T) {
	if err := quick.Check(func(x, y float32) bool {
		a, aErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.FloatLit{F: float64(x)},
			RHS: &ast.FloatLit{F: float64(y)},
			Op:  parser.MINUS,
		})
		if aErr != nil {
			t.Fatal(aErr)
		}
		b, bErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.FloatLit{F: 0},
				RHS: &ast.FloatLit{F: float64(y)},
				Op:  parser.MINUS,
			},
			RHS: &ast.FloatLit{F: float64(x)},
			Op:  parser.PLUS,
		})
		if bErr != nil {
			t.Fatal(bErr)
		}
		return cmp.Equal(a, b)
	}, nil); err != nil {
		t.Error(err)
	}
}

func TestConstFoldQuickMixedAddSub(t *testing.T) {
	if err := quick.Check(func(x int32, y float32) bool {
		a, aErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.IntLit{I: int64(x)},
			RHS: &ast.FloatLit{F: float64(y)},
			Op:  parser.MINUS,
		})
		if aErr != nil {
			t.Fatal(aErr)
		}
		b, bErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.FloatLit{F: 0},
				RHS: &ast.FloatLit{F: float64(y)},
				Op:  parser.MINUS,
			},
			RHS: &ast.IntLit{I: int64(x)},
			Op:  parser.PLUS,
		})
		if bErr != nil {
			t.Fatal(bErr)
		}
		return cmp.Equal(a, b)
	}, nil); err != nil {
		t.Error(err)
	}
}

var cmpFloat = cmp.Comparer(func(x, y float64) bool {
	delta := math.Abs(x - y)
	mean := math.Abs(x+y) / 2.0
	return delta/mean < 0.00001
})

func TestConstFoldQuickFloatMulDiv(t *testing.T) {
	if err := quick.Check(func(x, y float32) bool {
		a, aErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.FloatLit{F: float64(x)},
			RHS: &ast.FloatLit{F: float64(y)},
			Op:  parser.DIV,
		})
		if aErr != nil {
			t.Fatal(aErr)
		}
		b, bErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.FloatLit{F: 1},
				RHS: &ast.FloatLit{F: float64(y)},
				Op:  parser.DIV,
			},
			RHS: &ast.FloatLit{F: float64(x)},
			Op:  parser.MUL,
		})
		if bErr != nil {
			t.Fatal(bErr)
		}
		return cmp.Equal(a, b, cmpFloat)
	}, nil); err != nil {
		t.Error(err)
	}
}

func positiveInt(r *rand.Rand) int32 {
	v := r.Int31()
	if v == 0 {
		return 1
	}
	return v
}

func TestConstFoldQuickIntModAddition(t *testing.T) {
	values := func(args []reflect.Value, r *rand.Rand) {
		args[0] = reflect.ValueOf(positiveInt(r))
		args[1] = reflect.ValueOf(positiveInt(r))
		args[2] = reflect.ValueOf(positiveInt(r))
	}
	cfg := &quick.Config{
		Values: values,
	}
	if err := quick.Check(func(x, y, z int32) bool {
		a, aErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.IntLit{I: int64(x)},
				RHS: &ast.IntLit{I: int64(y)},
				Op:  parser.PLUS,
			},
			RHS: &ast.IntLit{I: int64(z)},
			Op:  parser.MOD,
		})
		if aErr != nil {
			t.Fatal(aErr)
		}
		b, bErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.BinaryExpr{
					LHS: &ast.IntLit{I: int64(x)},
					RHS: &ast.IntLit{I: int64(z)},
					Op:  parser.MOD,
				},
				RHS: &ast.BinaryExpr{
					LHS: &ast.IntLit{I: int64(y)},
					RHS: &ast.IntLit{I: int64(z)},
					Op:  parser.MOD,
				},
				Op: parser.PLUS,
			},
			RHS: &ast.IntLit{I: int64(z)},
			Op:  parser.MOD,
		})
		if bErr != nil {
			t.Fatal(bErr)
		}
		return cmp.Equal(a, b)
	}, cfg); err != nil {
		t.Error(err)
	}
}

func positiveFloat(r *rand.Rand) float32 {
	v := r.Float32()
	if v == 0.0 {
		return 1.0
	}
	return v
}

func TestConstFoldQuickFloatModAddition(t *testing.T) {
	values := func(args []reflect.Value, r *rand.Rand) {
		args[0] = reflect.ValueOf(positiveFloat(r))
		args[1] = reflect.ValueOf(positiveFloat(r))
		args[2] = reflect.ValueOf(positiveFloat(r))
	}
	cfg := &quick.Config{
		Values: values,
	}
	if err := quick.Check(func(x, y, z float32) bool {
		a, aErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.FloatLit{F: float64(x)},
				RHS: &ast.FloatLit{F: float64(y)},
				Op:  parser.PLUS,
			},
			RHS: &ast.FloatLit{F: float64(z)},
			Op:  parser.MOD,
		})
		if aErr != nil {
			t.Fatal(aErr)
		}
		b, bErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.BinaryExpr{
					LHS: &ast.FloatLit{F: float64(x)},
					RHS: &ast.FloatLit{F: float64(z)},
					Op:  parser.MOD,
				},
				RHS: &ast.BinaryExpr{
					LHS: &ast.FloatLit{F: float64(y)},
					RHS: &ast.FloatLit{F: float64(z)},
					Op:  parser.MOD,
				},
				Op: parser.PLUS,
			},
			RHS: &ast.FloatLit{F: float64(z)},
			Op:  parser.MOD,
		})
		if bErr != nil {
			t.Fatal(bErr)
		}
		return cmp.Equal(a, b)
	}, cfg); err != nil {
		t.Error(err)
	}
}

func TestConstFoldQuickMixedPowProduct(t *testing.T) {
	values := func(args []reflect.Value, r *rand.Rand) {
		args[0] = reflect.ValueOf(positiveFloat(r))
		args[1] = reflect.ValueOf(positiveInt(r))
		args[2] = reflect.ValueOf(positiveInt(r))
	}
	cfg := &quick.Config{
		Values: values,
	}
	if err := quick.Check(func(x float32, y, z int32) bool {
		a, aErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.FloatLit{F: float64(x)},
			RHS: &ast.BinaryExpr{
				LHS: &ast.IntLit{I: int64(y)},
				RHS: &ast.IntLit{I: int64(z)},
				Op:  parser.PLUS,
			},
			Op: parser.POW,
		})
		if aErr != nil {
			t.Fatal(aErr)
		}
		b, bErr := opt.Optimise(&ast.BinaryExpr{
			LHS: &ast.BinaryExpr{
				LHS: &ast.FloatLit{F: float64(x)},
				RHS: &ast.IntLit{I: int64(y)},
				Op:  parser.POW,
			},
			RHS: &ast.BinaryExpr{
				LHS: &ast.FloatLit{F: float64(x)},
				RHS: &ast.IntLit{I: int64(z)},
				Op:  parser.POW,
			},
			Op: parser.MUL,
		})
		if bErr != nil {
			t.Fatal(bErr)
		}
		return cmp.Equal(a, b)
	}, cfg); err != nil {
		t.Error(err)
	}
}
