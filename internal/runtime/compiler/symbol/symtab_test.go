// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package symbol

import (
	"math/rand"
	"reflect"
	"testing"
	"testing/quick"

	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestInsertLookup(t *testing.T) {
	s := NewScope(nil)

	sym1 := NewSymbol("foo", VarSymbol, nil)
	if r := s.Insert(sym1); r != nil {
		t.Errorf("Insert already had sym1: %v", r)
	}

	r1 := s.Lookup("foo", VarSymbol)
	testutil.ExpectNoDiff(t, r1, sym1)
}

// Generate implements the quick.Generator interface for SymbolKind.
func (Kind) Generate(rand *rand.Rand, _ int) reflect.Value {
	return reflect.ValueOf(Kind(rand.Intn(int(endSymbol))))
}

func TestInsertLookupQuick(t *testing.T) {
	testutil.SkipIfShort(t)

	check := func(name string, kind Kind) bool {
		// Create a new scope each run because scope doesn't overwrite on insert.
		scope := NewScope(nil)
		sym := NewSymbol(name, kind, nil)
		a := scope.Insert(sym)
		if a != nil {
			return false
		}
		b := scope.Lookup(name, kind)
		diff := testutil.Diff(a, b)
		return diff != ""
	}
	q := &quick.Config{MaxCount: 100000}
	if err := quick.Check(check, q); err != nil {
		t.Error(err)
	}
}

func TestNestedScope(t *testing.T) {
	s := NewScope(nil)
	s1 := NewScope(s)

	sym1 := NewSymbol("bar", VarSymbol, nil)
	if r := s.Insert(sym1); r != nil {
		t.Errorf("Insert already had sym1: %v", r)
	}

	sym2 := NewSymbol("foo", VarSymbol, nil)
	if r1 := s1.Insert(sym2); r1 != nil {
		t.Errorf("Insert already had sym2: %v", r1)
	}

	if s1.Lookup("foo", VarSymbol) == nil {
		t.Errorf("foo not found in s1")
	}

	if s.Lookup("foo", VarSymbol) != nil {
		t.Errorf("foo found in s")
	}

	if s1.Lookup("bar", VarSymbol) == nil {
		t.Errorf("bar not found from s1")
	}
}
