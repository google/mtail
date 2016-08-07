// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"testing"

	"github.com/kylelemons/godebug/pretty"
)

func TestLookupSymbol(t *testing.T) {
	// s := &symbol{"foo", IDSymbol, nil, position{"-", 1, 1, 3}, 0}

	// sc := &scope{}

	// Construct a dodgy symbol table
	tab := &SymbolTable{}
	tab.EnterScope(nil)
	s := tab.Add("foo", IDSymbol, position{"-", 1, 1, 3})
	// *tab = append(*tab, sc)
	// (*(*tab)[0])["foo"][IDSymbol] = s

	r, ok := tab.Lookup("foo", IDSymbol)
	if !ok {
		t.Errorf("Couldn't find symbol.")
	}
	diff := pretty.Compare(s, r)
	if len(diff) > 0 {
		t.Errorf("didn't get back the right symbol\n%s", diff)
	}

	r, ok = tab.Lookup("bar", IDSymbol)
	if ok {
		t.Errorf("Shold not have found bar.")
	}
	r, ok = tab.Lookup("foo", CaprefSymbol)
	if ok {
		t.Errorf("Should not have found foo: %v", r)
	}

	tab.EnterScope(nil)
	s1 := tab.Add("foo", IDSymbol, position{"-", 2, 1, 3})

	r, ok = tab.Lookup("foo", IDSymbol)
	if !ok {
		t.Errorf("Couldn't find symbol.")
	}
	diff = pretty.Compare(s1, r)
	if len(diff) > 0 {
		t.Errorf("didn't get back the right symbol\n%s", diff)
	}

}
