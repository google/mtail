// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package types

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
	case *Variable:
		r2, ok := t2.(*Variable)
		if !ok {
			return occursInType(t1, t2)
		}
		return t1 == r2
	case *Operator:
		t2, ok := t2.(*Operator)
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
	nextVariableIDMu sync.Mutex
	nextVariableID   int
)

// Variable represents an unbound type variable in the type system.
type Variable struct {
	ID int

	instanceMu sync.RWMutex
	Instance   *Type
}

// NewVariable constructs a new unique TypeVariable.
func NewVariable() *Variable {
	nextVariableIDMu.Lock()
	id := nextVariableID
	nextVariableID++
	nextVariableIDMu.Unlock()
	return &Variable{ID: id}
}

// Root returns an exemplar of this TypeVariable, in this case the root of the unification tree.
func (t *Variable) Root() Type {
	t.instanceMu.Lock()
	defer t.instanceMu.Unlock()
	if t.Instance == nil {
		return t
	}
	r := (*t.Instance).Root()
	t.Instance = &r
	return r
}

func (t *Variable) String() string {
	t.instanceMu.RLock()
	defer t.instanceMu.RUnlock()
	if t.Instance != nil {
		return (*t.Instance).String()
	}
	return fmt.Sprintf("typeVar%d", t.ID)

}

// SetInstance sets the exemplar instance of this TypeVariable, during
// unification.
func (t *Variable) SetInstance(t1 *Type) {
	t.instanceMu.Lock()
	defer t.instanceMu.Unlock()
	t.Instance = t1
}

// Operator represents a type scheme in the type system.
type Operator struct {
	// Name is a common name for this operator
	Name string
	// Args is the sequence of types that are parameters to this type.  They
	// may be fully bound type operators, or partially defined (i.e. contain
	// TypeVariables) in which case they represent polymorphism in the operator
	// they are argyments to.
	Args []Type
}

// Root returns an exemplar of a TypeOperator, i.e. itself.
func (t *Operator) Root() Type {
	return t
}

func (t *Operator) String() (s string) {
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
func Function(args ...Type) *Operator {
	return &Operator{"→", args}
}

// IsFunction returns true if the given type is a Function type.
func IsFunction(t Type) bool {
	if v, ok := t.(*Operator); ok {
		return v.Name == "→"
	}
	return false
}

// Dimension is a convenience method which instantiates a new Dimension type
// scheme, with the given args as the dimensions of the type.
func Dimension(args ...Type) *Operator {
	return &Operator{"⨯", args}
}

// IsDimension returns true if the given type is a Dimension type.
func IsDimension(t Type) bool {
	if v, ok := t.(*Operator); ok {
		return v.Name == "⨯"
	}
	return false
}

// IsComplete returns true if the type and all its arguments have non-variable exemplars.
func IsComplete(t Type) bool {
	switch v := t.Root().(type) {
	case *Variable:
		return false
	case *Operator:
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
	Undef   = &Operator{"Undef", []Type{}}
	Error   = &Operator{"Error", []Type{}}
	None    = &Operator{"None", []Type{}}
	Bool    = &Operator{"Bool", []Type{}}
	Int     = &Operator{"Int", []Type{}}
	Float   = &Operator{"Float", []Type{}}
	String  = &Operator{"String", []Type{}}
	Pattern = &Operator{"Pattern", []Type{}}
	// TODO(jaq): use composite type so we can typecheck the bucket directly, e.g. hist[j] = i
	Buckets = &Operator{"Buckets", []Type{}}
)

// Builtins is a mapping of the builtin language functions to their type definitions.
var Builtins = map[string]Type{
	"int":         Function(NewVariable(), Int),
	"bool":        Function(NewVariable(), Bool),
	"float":       Function(NewVariable(), Float),
	"string":      Function(NewVariable(), String),
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
	mappings := make(map[*Variable]*Variable)

	var freshRec func(Type) Type
	freshRec = func(tp Type) Type {
		p := tp.Root()
		switch p1 := p.(type) {
		case *Variable:
			if _, ok := mappings[p1]; !ok {
				mappings[p1] = NewVariable()
			}
			return mappings[p1]
		case *Operator:
			args := make([]Type, 0, len(p1.Args))
			for _, arg := range p1.Args {
				args = append(args, freshRec(arg))
			}
			return &Operator{p1.Name, args}
		default:
			glog.V(1).Infof("Unexpected type p1: %v", p1)
		}
		return tp
	}
	return freshRec(t)
}

func occursIn(v *Variable, types []Type) bool {
	for _, t2 := range types {
		if occursInType(v, t2) {
			return true
		}
	}
	return false
}

func occursInType(v *Variable, t2 Type) bool {
	root := t2.Root()
	if Equals(root, v) {
		return true
	}
	if to, ok := root.(*Operator); ok {
		return occursIn(v, to.Args)
	}
	return false
}

// TypeError describes an error in which a type was expected, but another was encountered.
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
	case *Variable:
		switch b2 := b1.(type) {
		case *Variable:
			if a2.ID != b2.ID {
				glog.V(2).Infof("Making %q type %q", a2, b1)
				a2.SetInstance(&b1)
				return nil
			}
		case *Operator:
			if occursInType(a2, b2) {
				return fmt.Errorf("recursive unification on %v and %v", a2, b2)
			}
			glog.V(2).Infof("Making %q type %q", a2, b1)
			a2.SetInstance(&b1)
			return nil
		}
	case *Operator:
		switch b2 := b1.(type) {
		case *Variable:
			err := Unify(b, a)
			if err != nil {
				// We flipped the args, flip them back.
				if e, ok := err.(*TypeError); ok {
					return &TypeError{e.received, e.expected}
				}
			}
			return err

		case *Operator:
			if len(a2.Args) != len(b2.Args) {
				return &TypeError{a2, b2}
			}
			if a2.Name != b2.Name {
				t := LeastUpperBound(a, b)
				glog.V(2).Infof("Got LUB = %q", t)
				if t == Error {
					return &TypeError{a2, b2}
				}
				// if !Equals(t, a2) {
				// 	a2.SetInstance(&t)
				// }
				// if !Equals(t, b2) {
				// 	b2.SetInstance(&t)
				// }
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

// LeastUpperBound returns the smallest type that may contain both parameter types.
func LeastUpperBound(a, b Type) Type {
	a1, b1 := a.Root(), b.Root()
	glog.V(2).Infof("Computing LUB(%q, %q)", a1, b1)

	if Equals(a1, b1) {
		return a1
	}
	// If either is a TypeVariable, the other is the lub
	if _, ok := a1.(*Variable); ok {
		return b1
	}
	if _, ok := b1.(*Variable); ok {
		return a1
	}
	// If either is Undef, other is the lub
	if Equals(a1, Undef) {
		return b1
	}
	if Equals(b1, Undef) {
		return a1
	}
	// Easy substitutions
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
	if (Equals(a1, Pattern) && Equals(b1, Bool)) ||
		(Equals(a1, Bool) && Equals(b1, Pattern)) {
		return Bool
	}
	if (Equals(a1, Bool) && Equals(b1, Int)) ||
		(Equals(a1, Int) && Equals(b1, Bool)) {
		return Int
	}
	// A string can be a pattern, but not vice versa.
	if (Equals(a1, String) && Equals(b1, Pattern)) ||
		(Equals(a1, Pattern) && Equals(b1, String)) {
		return Pattern
	}
	// A pattern and an Int are Bool
	if (Equals(a1, Pattern) && Equals(b1, Int)) ||
		(Equals(a1, Int) && Equals(b1, Pattern)) {
		return Bool
	}
	return Error
}

// inferCaprefType determines a type for a capturing group, based on contents
// of that capture group.
func InferCaprefType(re *syntax.Regexp, cap int) Type {
	group := getCaptureGroup(re, cap)
	if group == nil {
		return None
	}

	if group.Op != syntax.OpAlternate {
		return inferGroupType(group)
	}

	subType := Type(Undef)
	for _, sub := range group.Sub {
		subType = LeastUpperBound(subType, inferGroupType(sub))
	}
	return subType
}

func inferGroupType(group *syntax.Regexp) Type {
	switch {
	case groupOnlyMatches(group, "+-"):
		return String
	case groupOnlyMatches(group, "+-0123456789"):
		// Must be at least one digit in the group.
		if !groupOnlyMatches(group, "0123456789") {
			return String
		}
		return Int
	case groupOnlyMatches(group, "+-0123456789.eE"):
		// Only one decimal point allowed.
		if strings.Count(group.String(), ".") > 1 {
			return String
		}
		return Float
	}
	return String
}

// getCaptureGroup returns the Regexp node of the capturing group numbered cap
// in re.
func getCaptureGroup(re *syntax.Regexp, cap int) *syntax.Regexp {
	if re.Op == syntax.OpCapture && re.Cap == cap {
		return re.Sub[0]
	}
	for _, sub := range re.Sub {
		r := getCaptureGroup(sub, cap)
		if r != nil {
			return r
		}
	}
	return nil
}

// groupOnlyMatches returns true iff group only matches runes in s.
func groupOnlyMatches(group *syntax.Regexp, s string) bool {
	switch group.Op {
	case syntax.OpLiteral:
		for _, r := range group.Rune {
			if !strings.ContainsRune(s, r) {
				return false
			}
		}
		return true

	case syntax.OpCharClass:
		for i := 0; i < len(group.Rune); i += 2 {
			lo, hi := group.Rune[i], group.Rune[i+1]
			for r := lo; r <= hi; r++ {
				if !strings.ContainsRune(s, r) {
					return false
				}
			}
		}
		return true

	case syntax.OpStar, syntax.OpPlus, syntax.OpRepeat, syntax.OpQuest, syntax.OpCapture:
		return groupOnlyMatches(group.Sub[0], s)

	case syntax.OpConcat, syntax.OpAlternate:
		for _, sub := range group.Sub {
			if !groupOnlyMatches(sub, s) {
				return false
			}
		}

	default:
		return false
	}
	return true
}

// isErrorType indicates that a given type is the result of a type error.
func IsErrorType(t Type) bool {
	if o, ok := t.(*Operator); ok {
		if o.Name == "Error" {
			return true
		}
	}
	return false
}
