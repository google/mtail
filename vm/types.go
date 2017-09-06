// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
	"strings"
	"sync"

	"github.com/go-test/deep"
	"github.com/golang/glog"
)

type Type interface {
	Root() Type
	String() string
}

func Equals(t1, t2 Type) bool {
	return deep.Equal(t1, t2) == nil
}

var (
	nextVariableId   int
	nextVariableIdMu sync.Mutex
)

type TypeVariable struct {
	Id         int
	Instance   *Type
	instanceMu sync.RWMutex
}

func NewTypeVariable() *TypeVariable {
	nextVariableIdMu.Lock()
	id := nextVariableId
	nextVariableId += 1
	nextVariableIdMu.Unlock()
	return &TypeVariable{Id: id}
}

func (t *TypeVariable) Root() Type {
	t.instanceMu.Lock()
	defer t.instanceMu.Unlock()
	if t.Instance == nil {
		return t
	} else {
		r := (*t.Instance).Root()
		t.Instance = &r
		return r
	}
}

func (t *TypeVariable) String() string {
	t.instanceMu.RLock()
	defer t.instanceMu.RUnlock()
	if t.Instance != nil {
		return (*t.Instance).String()
	}
	return fmt.Sprintf("typeVar%d", t.Id)

}

func (t *TypeVariable) SetInstance(t1 *Type) {
	t.instanceMu.Lock()
	defer t.instanceMu.Unlock()
	t.Instance = t1
}

type TypeOperator struct {
	Name string
	Args []Type
}

func (t *TypeOperator) Root() Type {
	return t
}

func (t *TypeOperator) String() string {
	s := t.Name
	for _, a := range t.Args {
		s += " " + a.String()
	}
	return s
}

func Function(args ...Type) Type {
	return &TypeOperator{"→", args}
}

// Builtin types
var (
	Undef  = &TypeOperator{"Undef", []Type{}}
	Error  = &TypeOperator{"Error", []Type{}}
	None   = &TypeOperator{"None", []Type{}}
	Bool   = &TypeOperator{"Bool", []Type{}}
	Int    = &TypeOperator{"Int", []Type{}}
	Float  = &TypeOperator{"Float", []Type{}}
	String = &TypeOperator{"String", []Type{}}
)

var Builtins = map[string]Type{
	"int":         Function(NewTypeVariable(), Int),
	"bool":        Function(NewTypeVariable(), Bool),
	"float":       Function(NewTypeVariable(), Float),
	"string":      Function(NewTypeVariable(), String),
	"timestamp":   Function(Int),
	"len":         Function(String, Int),
	"settime":     Function(Int, None),
	"strptime":    Function(String, None),
	"strtol":      Function(String, Int),
	"tolower":     Function(String, String),
	"getfilename": Function(String),
}

func FreshType(t Type, nongeneric []Type) Type {
	mappings := make(map[*TypeVariable]*TypeVariable, 0)

	var freshRec func(Type) Type
	freshRec = func(tp Type) Type {
		p := tp.Root()
		switch p1 := p.(type) {
		case *TypeVariable:
			if isGeneric(p1, nongeneric) {
				if _, ok := mappings[p1]; !ok {
					mappings[p1] = NewTypeVariable()
				}
				return mappings[p1]
			} else {
				return p1
			}
		case *TypeOperator:
			args := make([]Type, len(p1.Args))
			for _, arg := range p1.Args {
				args = append(args, freshRec(arg))
			}
			return &TypeOperator{p1.Name, args}
		}
		return nil
	}
	return freshRec(t)
}

func isGeneric(v *TypeVariable, nongeneric []Type) bool {
	return !occursIn(v, nongeneric)
}

func occursIn(v *TypeVariable, types []Type) bool {
	for _, t2 := range types {
		if occursInType(v, t2) {
			return true
		}
	}
	return false
}

func occursInType(v *TypeVariable, t2 Type) bool {
	glog.Infof("v %v t2 %v", v, t2)
	// if t2 == nil {
	// 	return false
	// }
	root := t2.Root()
	if Equals(root, v) {
		return true
	}
	if to, ok := root.(*TypeOperator); ok {
		return occursIn(v, to.Args)
	}
	return false
}

type TypeError struct {
	expected Type
	received Type
}

func (e *TypeError) Error() string {
	return fmt.Sprintf("type mismatch; expected %q received %q", e.expected, e.received)
}

// Unify performs type unification of both parameter Types.  It returns the
// least upper bound of both types, the smallest type that is capable of
// representing both parameters.  If either type is a type variable, then that
// variable is unified with the LUB.  In reporting errors, it is assumed that a
// is the expected type and b is the type observed.
func Unify(a, b Type) error {
	glog.V(2).Infof("Unifying a %v , b %v", a, b)
	a1, b1 := a.Root(), b.Root()
	switch a2 := a1.(type) {
	case *TypeVariable:
		switch b2 := b1.(type) {
		case *TypeVariable:
			if a2.Id != b2.Id {
				glog.V(2).Infof("Making %q type %q", a2, b1)
				a2.SetInstance(&b1)
				return nil
			}
		case *TypeOperator:
			glog.Infof("b1 is a type operator thta looks liek %v", b2)
			if occursInType(a2, b2) {
				return fmt.Errorf("Recursive unification %v %v", a2, b2)
			}
			glog.V(2).Infof("Making %q type %q", a2, b1)
			a2.SetInstance(&b1)
			return nil
		}
	case *TypeOperator:
		switch b2 := b1.(type) {
		case *TypeVariable:
			err := Unify(b, a)
			if err != nil {
				// We flipped the args, flip them back.
				if e, ok := err.(*TypeError); ok {
					return &TypeError{e.received, e.expected}
				}
			}
			return err

		case *TypeOperator:
			glog.Infof("a2, b2: %q %q", a2, b2)
			if len(a2.Args) != len(b2.Args) {
				return &TypeError{a2, b2}
			}
			if a2.Name != b2.Name {
				t := leastUpperBound(a, b)
				if t == Error {
					return &TypeError{a2, b2}
				}
				a, b = t, t
				glog.Infof("post Lub a %v b %v", a, b)
				return nil
			}
			for i, argA := range a2.Args {
				glog.Infof("a and b: %q %q", argA, b2.Args[i])
				err := Unify(argA, b2.Args[i])
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func leastUpperBound(a, b Type) Type {
	glog.Infof("Lub a %v b %v", a, b)
	a1, b1 := a.Root(), b.Root()

	if Equals(a1, b1) {
		return a1
	}
	// least upper bound
	if (Equals(a1, Float) && Equals(b1, Int)) ||
		(Equals(b1, Float) && Equals(a1, Int)) {
		glog.Infof("Lub is Float")
		return Float
	}
	if (Equals(a1, String) && Equals(b1, Int)) ||
		(Equals(b1, String) && Equals(a1, Int)) ||
		(Equals(a1, String) && Equals(b1, Float)) ||
		(Equals(b1, String) && Equals(a1, Float)) {
		return String
	}
	return Error
}

// inferCaprefType determines a type for a capturing group, based on contents
// of that capture group.
func inferCaprefType(re *syntax.Regexp, cap int) Type {
	group := getCaptureGroup(re, cap)
	if group == nil {
		return None
	}
	switch {
	case groupOnlyMatches(group, "+-0123456789"):
		return Int
	case groupOnlyMatches(group, "+-0123456789.eE"):
		return Float
	}
	// TODO: String.  Current behaviour of mtail before types is assume all
	// matches are usable in arithmetic expressions.
	return Int
}

// getCaptureGroup returns the Regexp node of the capturing group numbered cap
// in re.
func getCaptureGroup(re *syntax.Regexp, cap int) *syntax.Regexp {
	if re.Op == syntax.OpCapture && re.Cap == cap {
		return re
	}
	for _, sub := range re.Sub {
		r := getCaptureGroup(sub, cap)
		if r != nil {
			return r
		}
	}
	return nil
}

// groupOnlyMatches returns true iff re only matches for runes in the s.
func groupOnlyMatches(re *syntax.Regexp, s string) bool {
	switch re.Op {
	case syntax.OpLiteral:
		for _, r := range re.Rune {
			if !strings.ContainsRune(s, r) {
				return false
			}
		}
		return true

	case syntax.OpCharClass:
		for i := 0; i < len(re.Rune); i += 2 {
			lo, hi := re.Rune[i], re.Rune[i+1]
			for r := lo; r <= hi; r++ {
				if !strings.ContainsRune(s, r) {
					return false
				}
			}
		}
		return true

	case syntax.OpStar, syntax.OpPlus, syntax.OpRepeat, syntax.OpQuest, syntax.OpCapture:
		return groupOnlyMatches(re.Sub[0], s)

	case syntax.OpConcat, syntax.OpAlternate:
		for _, sub := range re.Sub {
			if !groupOnlyMatches(sub, s) {
				return false
			}
		}

	default:
		return false
	}
	return true
}
