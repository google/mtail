// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

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
	loc     *position   // Source file position
	addr    int         // Address offset in another structure
	typ     Type        // Type of this symbol
}

type scope map[string][]*symbol

type SymbolTable []*scope

func (s *SymbolTable) EnterScope(sc *scope) *scope {
	if sc == nil {
		sc = &scope{}
	}
	*s = append(*s, sc)
	return sc
}

func (s *SymbolTable) ExitScope() {
	if len(*s) > 1 {
		*s = (*s)[:len(*s)-1]
	}
}

func (s *SymbolTable) CurrentScope() *scope {
	return (*s)[len(*s)-1]
}

func (s *SymbolTable) Lookup(name string, kind symtype) (*symbol, bool) {
	for i := len(*s) - 1; i >= 0; i-- {
		if r, ok := (*(*s)[i])[name]; ok && r[kind] != nil {
			return r[kind], ok
		}
	}
	return nil, false
}

func (s *SymbolTable) Add(name string, kind symtype, loc *position) (sym *symbol) {
	sym = &symbol{name, kind, nil, loc, 0, Int}
	cs := s.CurrentScope()
	if _, ok := (*cs)[name]; !ok {
		(*cs)[name] = make([]*symbol, endSymbol)
	}
	(*cs)[name][kind] = sym
	return sym
}
