// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package code contains the bytecode instructions for the mtail virtual machine.
package code

type Opcode int

const (
	Bad        Opcode = iota // Invalid instruction, indicates a bug in the generator.
	Stop                     // Stop the program, ending processing of this input.
	Match                    // Match a regular expression against input, and set the match register.
	Smatch                   // Match a regular expression against top of stack, and set the match register.
	Cmp                      // Compare two values on the stack and set the match register.
	Jnm                      // Jump if no match.
	Jm                       // Jump if match.
	Jmp                      // Unconditional jump
	Inc                      // Increment a variable value
	Dec                      // Decrement a variable value
	Strptime                 // Parse into the timestamp register
	Timestamp                // Return value of timestamp register onto TOS.
	Settime                  // Set timestamp register to value at TOS.
	Push                     // Push operand onto stack
	Capref                   // Push capture group reference at operand onto stack
	Str                      // Push string constant at operand onto stack
	Sset                     // Set a string variable value.
	Iset                     // Set a variable value
	Iadd                     // Add top values on stack and push to stack
	Isub                     // Subtract top value from second top value on stack, and push to stack.
	Imul                     // Multiply top values on stack and push to stack
	Idiv                     // Divide top value into second top on stack, and push
	Imod                     // Integer divide top value into second top on stack, and push remainder
	Ipow                     // Put second TOS to power of TOS, and push.
	And                      // Bitwise AND the 2 at top of stack, and push result
	Or                       // Bitwise OR the 2 at top of stack, and push result
	Xor                      // Bitwise XOR the 2 at top of stack, and push result
	Neg                      // Bitwise NOT the top of stack, and push result
	Not                      // Boolean NOT the top of stack, and push result
	Shl                      // Shift TOS left, push result
	Shr                      // Shift TOS right, push result
	Mload                    // Load metric at operand onto top of stack
	Dload                    // Pop `operand` keys and metric off stack, and push datum at metric[key,...] onto stack.
	Iget                     // Pop a datum off the stack, and push its integer value back on the stack.
	Fget                     // Pop a datum off the stack, and push its float value back on the stack.
	Sget                     // Pop a datum off the stack, and push its string value back on the stack.
	Tolower                  // Convert the string at the top of the stack to lowercase.
	Length                   // Compute the length of a string.
	Cat                      // string concatenation
	Setmatched               // Set "matched" flag
	Otherwise                // Only match if "matched" flag is false.
	Del                      // Pop `operand` keys and metric off stack, and remove the datum at metric[key,...] from memory
	Expire                   // Set the expiry duration of a datum, perfoming the same as del but after the expiry time passes.

	// Floating point ops.
	Fadd
	Fsub
	Fmul
	Fdiv
	Fmod
	Fpow
	Fset // Floating point assignment

	Getfilename // Push input.Filename onto the stack.

	// Conversions.
	I2f // int to float
	S2i // string to int
	S2f // string to float
	I2s // int to string
	F2s // float to string

	// Typed comparisons, behave the same as cmp but do no conversion.
	Icmp // integer compare
	Fcmp // floating point compare
	Scmp // string compare

	// String opcodes.
	Subst
	Rsubst

	lastOpcode
)

var opNames = map[Opcode]string{
	Stop:        "stop",
	Match:       "match",
	Smatch:      "smatch",
	Cmp:         "cmp",
	Jnm:         "jnm",
	Jm:          "jm",
	Jmp:         "jmp",
	Inc:         "inc",
	Strptime:    "strptime",
	Timestamp:   "timestamp",
	Settime:     "settime",
	Push:        "push",
	Capref:      "capref",
	Str:         "str",
	Sset:        "sset",
	Iset:        "iset",
	Iadd:        "iadd",
	Isub:        "isub",
	Imul:        "imul",
	Idiv:        "idiv",
	Imod:        "imod",
	Ipow:        "ipow",
	Shl:         "shl",
	Shr:         "shr",
	And:         "and",
	Or:          "or",
	Xor:         "xor",
	Not:         "not",
	Neg:         "neg",
	Mload:       "mload",
	Dload:       "dload",
	Iget:        "iget",
	Fget:        "fget",
	Sget:        "sget",
	Tolower:     "tolower",
	Length:      "length",
	Cat:         "cat",
	Setmatched:  "setmatched",
	Otherwise:   "otherwise",
	Del:         "del",
	Fadd:        "fadd",
	Fsub:        "fsub",
	Fmul:        "fmul",
	Fdiv:        "fdiv",
	Fmod:        "fmod",
	Fpow:        "fpow",
	Fset:        "fset",
	Getfilename: "getfilename",
	I2f:         "i2f",
	S2i:         "s2i",
	S2f:         "s2f",
	I2s:         "i2s",
	F2s:         "f2s",
	Icmp:        "icmp",
	Fcmp:        "fcmp",
	Scmp:        "scmp",
	Subst:       "subst",
	Rsubst:      "rsubst",
}

func (o Opcode) String() string {
	return opNames[o]
}
