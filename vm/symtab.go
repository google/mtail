// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import "github.com/golang/glog"

type symtype int

// symtype enumerates the types of symbols found in the program text.
const (
	IDSymbol     symtype = iota // Identifiers
	CaprefSymbol                // Capture group references
	DefSymbol                   // Definitions

	endSymbol
)

type symbol struct {
	name    string      // Symbol name
	kind    symtype     // Type
	binding interface{} // Binding to storage allocated
	loc     position    // Source file position
	addr    int         // Address offset in another structure
}

type scope map[string][]*symbol

type SymbolTable []*scope

func (s *SymbolTable) ScopeStart() (scope *scope) {
	scope := &scope{map[string][]*symbol{}}
	s = append(s, scope)
	return
}

func (s *SymbolTable) ScopeEnd() {
	s = s[:len(s)-1]
}

func (s *SymbolTable) CurrentScope() *scope {
	return s[len(s)-1]
}

func (s *SymbolTable) Lookup(name string, kind symtype) (*symbol, bool) {
	glog.Infof("looking up %s", name)
	if r, ok := s.symtab[name]; ok {
		return r[kind], ok
	}
	if s.parent != nil {
		glog.Info("going to parent")
		return s.parent.lookupSym(name, kind)
	}
	return nil, false
}

func (s *SymbolTable) Add(name string, kind symtype, loc position) (sym *symbol) {
	sym := &symbol{name, kind, nil, loc, 0}

}

func (s *scope) addSym(name string, kind symtype, binding interface{}, loc position) *symbol {
	sym := &symbol{name, kind, binding, loc, 0}
	if _, ok := s.symtab[name]; !ok {
		s.symtab[name] = make([]*symbol, endSymbol)
	}
	s.symtab[name][kind] = sym
	return sym
}
