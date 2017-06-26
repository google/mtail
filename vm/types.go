// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"reflect"
	"regexp/syntax"
	"strings"

	"github.com/golang/glog"
)

type Type interface {
	Root() Type
	String() string
}

func Equals(t1, t2 Type) bool {
	return reflect.DeepEqual(t1, t2)
}

var nextVariableId int

type TypeVariable struct {
	Id       int
	Instance *Type
}

func NewTypeVariable() Type {
	id := nextVariableId
	nextVariableId += 1
	return &TypeVariable{Id: id}
}

func (t *TypeVariable) Root() Type {
	if t.Instance == nil {
		return t
	} else {
		r := (*t.Instance).Root()
		t.Instance = &r
		return r
	}
}

func (t *TypeVariable) String() string {
	if t.Instance != nil {
		return (*t.Instance).String()
	}
	return fmt.Sprintf("typeVar%d", t.Id)

}

type TypeOperator struct {
	Name string
}

func (t *TypeOperator) Root() Type {
	return t
}

func (t *TypeOperator) String() string {
	return t.Name
}

// Builtin types
var (
	Undef  = &TypeOperator{"Undef"}
	None   = &TypeOperator{"None"}
	Int    = &TypeOperator{"Int"}
	Float  = &TypeOperator{"Float"}
	String = &TypeOperator{"String"}
)

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
				a2.Instance = &b1
				return a2
			}
		case *TypeOperator:
			glog.V(2).Infof("Making %q type %q", a2, b1)
			a2.Instance = &b1
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
