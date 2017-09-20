// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"testing"

	go_cmp "github.com/google/go-cmp/cmp"
)

func TestInsertLookup(t *testing.T) {
	s := NewScope(nil)

	sym1 := NewSymbol("foo", VarSymbol, nil)
	if r := s.Insert(sym1); r != nil {
		t.Errorf("Insert already had sym1: %v", r)
	}

	r1 := s.Lookup("foo")
	if diff := go_cmp.Diff(r1, sym1); diff != "" {
		t.Error(diff)
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

	if s1.Lookup("foo") == nil {
		t.Errorf("foo not found in s1")
	}

	if s.Lookup("foo") != nil {
		t.Errorf("foo found in s")
	}

	if s1.Lookup("bar") == nil {
		t.Errorf("bar not found from s1")
	}
}
