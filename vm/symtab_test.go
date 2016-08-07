// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"testing"

	"github.com/kylelemons/godebug/pretty"
)

func TestLookupSymbol(t *testing.T) {
	s := &symbol{"foo", IDSymbol, nil, position{"-", 1, 1, 1, 3}, 0}

	sc := &scope{map[string][]*symbol{}}

	// Construct a dodgy symbol table
	tab := &SymbolTable{&scope{map[string][]*symbol{}}}
	tab = append(tab, &scope)
	tab[0]["foo"] = s

	r, ok := tab.Lookup("foo", IDSymbol)
	if !ok {
		t.Errorf("Couldn't find symbol.")
	}
	diff := pretty.Compare(s, r)
	if len(diff) > 0 {
		t.Errorf("didn't get back the right symbol\n%s", diff)
	}
}
