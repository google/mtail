// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"fmt"
	"testing"
	"testing/quick"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
)

func TestKindHasString(t *testing.T) {
	for k := INVALID; k <= NL; k++ {
		if Kind(k).String() != mtailToknames[k-INVALID+3] {
			t.Errorf("kind string not match. expected %s, received %s", mtailToknames[k-INVALID], Kind(k).String())
		}
	}
}

func TestTokenString(t *testing.T) {
	if err := quick.Check(func(kind Kind, spelling string, pos position.Position) bool {
		tok := Token{Kind: kind, Spelling: spelling, Pos: pos}
		return tok.String() == fmt.Sprintf("%s(%q,%s)", kind.String(), spelling, pos.String())
	}, nil); err != nil {
		t.Error(err)
	}
}
