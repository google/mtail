// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

type symtype int

const (
	IdSymbol symtype = iota
	CaprefSymbol
	DefSymbol

	endSymbol
)

type symbol struct {
	name    string
	kind    symtype
	binding interface{}
	loc     Position
	addr    int
}

type scope struct {
	parent *scope
	symtab map[string][]*symbol
}

func (s *scope) lookupSym(name string, kind symtype) (*symbol, bool) {
	r, ok := s.symtab[name]
	if !ok && s.parent != nil {
		return s.parent.lookupSym(name, kind)
	} else if ok {
		return r[kind], ok
	}
	return nil, ok
}

func (s *scope) addSym(name string, kind symtype, binding interface{}, loc Position) *symbol {
	sym := &symbol{name, kind, binding, loc, 0}
	if _, ok := s.symtab[name]; !ok {
		s.symtab[name] = make([]*symbol, endSymbol)
	}
	s.symtab[name][kind] = sym
	return sym
}
