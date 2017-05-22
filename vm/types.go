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
		return t.Instance.String()
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
