// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package vm provides a compiler and virtual machine environment for executing
// mtail programs.
package vm

import "fmt"

type opcode int

const (
	bad        opcode = iota // Invalid instruction, indicates a bug in the generator.
	match                    // Match a regular expression against input, and set the match register.
	smatch                   // Match a regular expression against top of stack, and set the match register.
	cmp                      // Compare two values on the stack and set the match register.
	jnm                      // Jump if no match.
	jm                       // Jump if match.
	jmp                      // Unconditional jump
	inc                      // Increment a variable value
	dec                      // Decrement a variable value
	strptime                 // Parse into the timestamp register
	timestamp                // Return value of timestamp register onto TOS.
	settime                  // Set timestamp register to value at TOS.
	push                     // Push operand onto stack
	capref                   // Push capture group reference at operand onto stack
	str                      // Push string constant at operand onto stack
	sset                     // Set a string variable value.
	iset                     // Set a variable value
	iadd                     // Add top values on stack and push to stack
	isub                     // Subtract top value from second top value on stack, and push to stack.
	imul                     // Multiply top values on stack and push to stack
	idiv                     // Divide top value into second top on stack, and push
	imod                     // Integer divide top value into second top on stack, and push remainder
	ipow                     // Put second TOS to power of TOS, and push.
	and                      // Bitwise AND the 2 at top of stack, and push result
	or                       // Bitwise OR the 2 at top of stack, and push result
	xor                      // Bitwise XOR the 2 at top of stack, and push result
	neg                      // Bitwise NOT the top of stack, and push result
	not                      // Boolean NOT the top of stack, and push result
	shl                      // Shift TOS left, push result
	shr                      // Shift TOS right, push result
	mload                    // Load metric at operand onto top of stack
	dload                    // Pop `operand` keys and metric off stack, and push datum at metric[key,...] onto stack.
	iget                     // Pop a datum off the stack, and push its integer value back on the stack.
	fget                     // Pop a datum off the stack, and push its float value back on the stack.
	sget                     // Pop a datum off the stack, and push its string value back on the stack.
	tolower                  // Convert the string at the top of the stack to lowercase.
	length                   // Compute the length of a string.
	cat                      // string concatenation
	setmatched               // Set "matched" flag
	otherwise                // Only match if "matched" flag is false.
	del                      //  Pop `operand` keys and metric off stack, and remove the datum at metric[key,...] from memory

	// Floating point ops
	fadd
	fsub
	fmul
	fdiv
	fmod
	fpow
	fset // Floating point assignment

	getfilename // Push input.Filename onto the stack.

	// Conversions
	i2f // int to float
	s2i // string to int
	s2f // string to float
	i2s // int to string
	f2s // float to string

	// Typed comparisons, behave the same as cmp but do no conversion.
	icmp // integer compare
	fcmp // floating point compare
	scmp // string compare
)

var opNames = map[opcode]string{
	match:       "match",
	smatch:      "smatch",
	cmp:         "cmp",
	jnm:         "jnm",
	jm:          "jm",
	jmp:         "jmp",
	inc:         "inc",
	strptime:    "strptime",
	timestamp:   "timestamp",
	settime:     "settime",
	push:        "push",
	capref:      "capref",
	str:         "str",
	sset:        "sset",
	iset:        "iset",
	iadd:        "iadd",
	isub:        "isub",
	imul:        "imul",
	idiv:        "idiv",
	imod:        "imod",
	ipow:        "ipow",
	shl:         "shl",
	shr:         "shr",
	and:         "and",
	or:          "or",
	xor:         "xor",
	not:         "not",
	neg:         "neg",
	mload:       "mload",
	dload:       "dload",
	iget:        "iget",
	fget:        "fget",
	sget:        "sget",
	tolower:     "tolower",
	length:      "length",
	cat:         "cat",
	setmatched:  "setmatched",
	otherwise:   "otherwise",
	del:         "del",
	fadd:        "fadd",
	fsub:        "fsub",
	fmul:        "fmul",
	fdiv:        "fdiv",
	fmod:        "fmod",
	fpow:        "fpow",
	fset:        "fset",
	getfilename: "getfilename",
	i2f:         "i2f",
	s2i:         "s2i",
	s2f:         "s2f",
	i2s:         "i2s",
	f2s:         "f2s",
	icmp:        "icmp",
	fcmp:        "fcmp",
	scmp:        "scmp",
}

var builtin = map[string]opcode{
	"getfilename": getfilename,
	"len":         length,
	"settime":     settime,
	"strptime":    strptime,
	"strtol":      s2i,
	"timestamp":   timestamp,
	"tolower":     tolower,
}

type instr struct {
	op   opcode
	opnd interface{}
}

// debug print for instructions
func (i instr) String() string {
	return fmt.Sprintf("{%s %v}", opNames[i.op], i.opnd)
}
