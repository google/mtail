// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"reflect"
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
	Int    = &TypeOperator{"Int"}
	Float  = &TypeOperator{"Float"}
	String = &TypeOperator{"String"}
	None   = &TypeOperator{"None"}
)

// Unify computes type unification of both parameter Types.  It computes the
// least upper bound of both types, the smallest type that is capable of
// representing both parameters, and stores the root exemplar of that type if
// either type is currently a free variable.
func Unify(a, b Type) Type {
	a1, b1 := a.Root(), b.Root()
	switch a2 := a1.(type) {
	case *TypeVariable:
		switch b2 := b1.(type) {
		case *TypeVariable:
			if a2.Id == b2.Id {
				return a2
			}
		case *TypeOperator:
			a2.Instance = &b1
			return b1
		}
	case *TypeOperator:
		switch b2 := b1.(type) {
		case *TypeVariable:
			return Unify(b, a)
		case *TypeOperator:
			if a2.Name != b2.Name {
				return None
			}
			return a2
		}
	}
	return None
}
