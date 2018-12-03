// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"fmt"

	"github.com/google/mtail/internal/vm/position"
)

// Kind enumerates the types of lexical tokens in a mtail program.
type TokenKind int

func (k TokenKind) String() string {
	return TokenKindName(k)
}

// Token describes a lexed Token from the input, containing its type, the
// original text of the Token, and its position in the input.
type Token struct {
	Kind     TokenKind
	Spelling string
	Pos      position.Position
}

func (t Token) String() string {
	return fmt.Sprintf("%s(%q,%s)", t.Kind, t.Spelling, t.Pos)
}
