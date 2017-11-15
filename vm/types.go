// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
	"strings"
	"sync"

	"github.com/golang/glog"
)

// Type represents a type in the mtail program.
type Type interface {
	// Root returns an exemplar Type after unification occurs.  If the type
	// system is complete after unification, Root will be a TypeOperator.
	Root() Type

	// String returns a string representation of a Type.
	String() string
}

// Equals compares two types, testing for equality
func Equals(t1, t2 Type) bool {
	t1, t2 = t1.Root(), t2.Root()
	switch t1 := t1.(type) {
	case *TypeVariable:
		r2, ok := t2.(*TypeVariable)
		if !ok {
			return occursInType(t1, t2)
		}
		return t1 == r2
	case *TypeOperator:
		t2, ok := t2.(*TypeOperator)
		if !ok {
			return false
		}
		if t1.Name != t2.Name {
			return false
		}
		if len(t1.Args) != len(t2.Args) {
			return false
		}
		for i := range t1.Args {
			if !Equals(t1.Args[i], t2.Args[2]) {
				return false
			}
		}
		return true
	}
	return true

}

var (
	nextVariableId   int
	nextVariableIdMu sync.Mutex
)

// TypeVariable represents an unbound type variable in the type system.
type TypeVariable struct {
	Id         int
	Instance   *Type
	instanceMu sync.RWMutex
}

// NewTypeVariable constructs a new unique TypeVariable.
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

// SetInstance sets the exemplar instance of this TypeVariable, during
// unification.
func (t *TypeVariable) SetInstance(t1 *Type) {
	t.instanceMu.Lock()
	defer t.instanceMu.Unlock()
	t.Instance = t1
}

// TypeOperator represents a type scheme in the type system.
type TypeOperator struct {
	// Name is a common name for this operator
	Name string
	// Args is the sequence of types that are parameters to this type.  They
	// may be fully bound type operators, or partially defined (i.e. contain
	// TypeVariables) in which case they represent polymorphism in the operator
	// they are argyments to.
	Args []Type
}

func (t *TypeOperator) Root() Type {
	return t
}

func (t *TypeOperator) String() (s string) {
	switch l := len(t.Args); {
	case l < 2:
		s = t.Name
		for _, a := range t.Args {
			s += " " + a.String()
		}
	default:
		s = t.Args[0].String()
		for _, a := range t.Args[1:] {
			s += t.Name + a.String()
		}
	}
	return s
}

// Function is a convenience method, which instantiates a new Function type
// scheme, with the given args as parameters.
func Function(args ...Type) *TypeOperator {
	return &TypeOperator{"→", args}
}

// IsFunction returns true if the given type is a Function type.
func IsFunction(t Type) bool {
	if v, ok := t.(*TypeOperator); ok {
		return v.Name == "→"
	}
	return false
}

// Dimension is a convenience method which instantiates a new Dimension type
// scheme, with the given args as the dimensions of the type.
func Dimension(args ...Type) *TypeOperator {
	return &TypeOperator{"⨯", args}
}

// IsDimension returns true if the given type is a Dimension type.
func IsDimension(t Type) bool {
	if v, ok := t.(*TypeOperator); ok {
		return v.Name == "⨯"
	}
	return false
}

// IsComplete returns true if the type and all its arguments have non-variable exemplars.
func IsComplete(t Type) bool {
	switch v := t.Root().(type) {
	case *TypeVariable:
		return false
	case *TypeOperator:
		for _, a := range v.Args {
			if !IsComplete(a) {
				return false
			}
		}
		return true
	}
	return false
}

// Builtin types
var (
	Undef   = &TypeOperator{"Undef", []Type{}}
	Error   = &TypeOperator{"Error", []Type{}}
	None    = &TypeOperator{"None", []Type{}}
	Bool    = &TypeOperator{"Bool", []Type{}}
	Int     = &TypeOperator{"Int", []Type{}}
	Float   = &TypeOperator{"Float", []Type{}}
	String  = &TypeOperator{"String", []Type{}}
	Pattern = &TypeOperator{"Pattern", []Type{}}
)

// Builtins is a mapping of the builtin language functions to their type definitions.
var Builtins = map[string]Type{
	"int":         Function(NewTypeVariable(), Int),
	"bool":        Function(NewTypeVariable(), Bool),
	"float":       Function(NewTypeVariable(), Float),
	"string":      Function(NewTypeVariable(), String),
	"timestamp":   Function(Int),
	"len":         Function(String, Int),
	"settime":     Function(Int, None),
	"strptime":    Function(String, String, None),
	"strtol":      Function(String, Int, Int),
	"tolower":     Function(String, String),
	"getfilename": Function(String),
}

// FreshType returns a new type from the provided type scheme, replacing any
// unbound type variables with new type variables.
func FreshType(t Type) Type {
	mappings := make(map[*TypeVariable]*TypeVariable)

	var freshRec func(Type) Type
	freshRec = func(tp Type) Type {
		p := tp.Root()
		switch p1 := p.(type) {
		case *TypeVariable:
			if _, ok := mappings[p1]; !ok {
				mappings[p1] = NewTypeVariable()
			}
			return mappings[p1]
		case *TypeOperator:
			args := make([]Type, 0, len(p1.Args))
			for _, arg := range p1.Args {
				args = append(args, freshRec(arg))
			}
			return &TypeOperator{p1.Name, args}
		default:
			glog.V(1).Infof("Unexpected type p1: %v", p1)
		}
		return tp
	}
	return freshRec(t)
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
	var estr, rstr string
	if IsComplete(e.expected) {
		estr = e.expected.String()
	} else {
		estr = "incomplete type"
	}
	if IsComplete(e.received) {
		rstr = e.received.String()
	} else {
		rstr = "incomplete type"
	}
	glog.V(2).Infof("type mismatch: expected %q received %q", e.expected, e.received)
	return fmt.Sprintf("type mismatch; expected %s received %s", estr, rstr)
}

// Unify performs type unification of both parameter Types.  It returns the
// least upper bound of both types, the smallest type that is capable of
// representing both parameters.  If either type is a type variable, then that
// variable is unified with the LUB.  In reporting errors, it is assumed that a
// is the expected type and b is the type observed.
func Unify(a, b Type) error {
	glog.V(2).Infof("Unifying %v and %v", a, b)
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
			if occursInType(a2, b2) {
				return fmt.Errorf("Recursive unification on %v and %v", a2, b2)
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
			if len(a2.Args) != len(b2.Args) {
				return &TypeError{a2, b2}
			}
			if a2.Name != b2.Name {
				t := LeastUpperBound(a, b)
				if t == Error {
					return &TypeError{a2, b2}
				}
				return nil
			}
			for i, argA := range a2.Args {
				err := Unify(argA, b2.Args[i])
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func LeastUpperBound(a, b Type) Type {
	a1, b1 := a.Root(), b.Root()

	if Equals(a1, b1) {
		return a1
	}
	// If either is a TypeVariable, the other is the lub
	if _, ok := a1.(*TypeVariable); ok {
		return b1
	}
	if _, ok := b1.(*TypeVariable); ok {
		return a1
	}
	if (Equals(a1, Float) && Equals(b1, Int)) ||
		(Equals(b1, Float) && Equals(a1, Int)) {
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
		if strings.Count(group.String(), ".") <= 1 {
			return Float
		}
		return String
	}
	return String
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
