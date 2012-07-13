// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

type symtype int

const (
	IntSymbol symtype = iota
	StringSymbol
	CaprefSymbol
	ScalarMetricSymbol
	DimensionedMetricSymbol
)

type symbol struct {
	name    string
	kind    symtype
	binding interface{}
	loc     Position
	addr    int
	level   int // nesting level
}

type scope struct {
	parent *scope
	level  int // nesting level
	symtab map[string]*symbol
}

func (s *scope) lookupSym(name string) (*symbol, bool) {
	r, ok := s.symtab[name]
	if !ok && s.parent != nil {
		return s.parent.lookupSym(name)
	}
	return r, ok
}

func (s *scope) addSym(name string, kind symtype, binding interface{}, loc Position) *symbol {
	sym := &symbol{name, kind, binding, loc, 0, s.level}
	s.symtab[name] = sym
	return sym
}
