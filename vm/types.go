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

func NewTypeVariable() Type {
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
	None   = &TypeOperator{"None", []Type{}}
	Int    = &TypeOperator{"Int", []Type{}}
	Float  = &TypeOperator{"Float", []Type{}}
	String = &TypeOperator{"String", []Type{}}
)

var Builtins = map[string]Type{
	"timestamp":   Function(Int),
	"len":         Function(String, Int),
	"settime":     Function(Int, None),
	"strptime":    Function(String, None),
	"strtol":      Function(String, Int),
	"tolower":     Function(String, String),
	"getfilename": Function(String),
}

// Unify performs type unification of both parameter Types.  It returns the
// least upper bound of both types, the smallest type that is capable of
// representing both parameters.  If either type is a type variable, then that
// variable is unified with the LUB.
func Unify(a, b Type) Type {
	a1, b1 := a.Root(), b.Root()
	switch a2 := a1.(type) {
	case *TypeVariable:
		switch b2 := b1.(type) {
		case *TypeVariable:
			if a2.Id == b2.Id {
				return a2
			} else {
				glog.V(2).Infof("Making %q type %q", a2, b1)
				a2.SetInstance(&b1)
				return a2
			}
		case *TypeOperator:
			glog.V(2).Infof("Making %q type %q", a2, b1)
			a2.SetInstance(&b1)
			return b1
		}
	case *TypeOperator:
		switch b2 := b1.(type) {
		case *TypeVariable:
			return Unify(b, a)
		case *TypeOperator:
			if Equals(a2, b2) {
				return a2
			}
			// least upper bound
			if (Equals(a2, Float) && Equals(b2, Int)) ||
				(Equals(b2, Float) && Equals(a2, Int)) {
				return Float
			}
			if (Equals(a2, String) && Equals(b2, Int)) ||
				(Equals(b2, String) && Equals(a2, Int)) ||
				(Equals(a2, String) && Equals(b2, Float)) ||
				(Equals(b2, String) && Equals(a2, Float)) {
				return String
			}

			if len(a2.Args) != len(b2.Args) {
				// TODO return error: glog.Errorf("Type mismatch: %q vs %q", a2, b2)
				return None
			}
			for i := range a2.Args {
				Unify(a2.Args[i], b2.Args[i])
			}
			return None
		}
	}
	return None
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
