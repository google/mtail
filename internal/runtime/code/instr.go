// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package code contains the bytecode instructions for the mtail virtual machine.
package code

import "fmt"

type Instr struct {
	Opcode     Opcode
	Operand    interface{}
	SourceLine int // Line number of the original source file, zero-based numbering.
}

// debug print for instructions.
func (i Instr) String() string {
	return fmt.Sprintf("{%s %v %d}", opNames[i.Opcode], i.Operand, i.SourceLine)
}
