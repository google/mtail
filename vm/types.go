// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import "fmt"

type Type interface {
	isType()
}

var nextVariableId int

type TypeVariable struct {
	Id       int
	Instance *Type
}

func (*TypeVariable) isType() {}

func NewTypeVariable() Type {
	id := nextVariableId
	nextVariableId += 1
	return &TypeVariable{Id: id}
}

func (t *TypeVariable) String() string {
	if t.Instance != nil {
		return fmt.Sprintf("%s", t.Instance)
	}
	return fmt.Sprintf("typeVar%d", t.Id)

}

type TypeOperator struct {
	Name string
}

func (*TypeOperator) isType() {}

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
// representing both parameters.
func Unify(a, b Type) Type {
	if a1, ok := a.(*TypeVariable); ok {
		if a != b {
			a1.Instance = &b
			return b
		}
	}
	return None
}
