// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

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

type TypeOperator struct {
	name string
}

func (*TypeOperator) isType() {}

// Builtin types
var (
	Int    = &TypeOperator{"Int"}
	Float  = &TypeOperator{"Float"}
	String = &TypeOperator{"String"}
	None   = &TypeOperator{"None"}
)
