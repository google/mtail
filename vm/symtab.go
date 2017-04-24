// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

type SymbolClass int

// SymbolClass enumerates the classes of symbols found in the program text.
const (
	IDSymbol     SymbolClass = iota // Identifiers
	CaprefSymbol                    // Capture group references
	DefSymbol                       // Definitions

	endSymbol
)

// symbol is an entry in the symbol table within a certain scope.
type symbol struct {
	name    string      // Symbol name
	class   SymbolClass // Type
	binding interface{} // Binding to storage allocated
	loc     *position   // Source file position
	addr    int         // Address offset in another structure
	typ     Type        // Type of this symbol
}

// scope maps an object name to a list of symbols with that name.  Objects with
// the same SymbolClass cannot exist at the same scope.  Objects with different
// SymbolClass may exist at the same scope.
type scope map[string][]*symbol

// SymbolTable is a stack of scopes.  As new scopes are entered, they are
// pushed onto the end of the stack.  As scopes are exited, they are removed
// from the stack.  References to each scope are held by the AST nodes that are
// contained within them, for speed of access when performing a lookup, and
// preventing garbage collection until the AST is no longer referenced.
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

func (s *SymbolTable) Lookup(name string, class SymbolClass) (*symbol, bool) {
	for i := len(*s) - 1; i >= 0; i-- {
		if r, ok := (*(*s)[i])[name]; ok && r[class] != nil {
			return r[class], ok
		}
	}
	return nil, false
}

func (s *SymbolTable) Add(name string, class SymbolClass, loc *position) (sym *symbol) {
	sym = &symbol{name, class, nil, loc, 0, Int}
	cs := s.CurrentScope()
	if _, ok := (*cs)[name]; !ok {
		(*cs)[name] = make([]*symbol, endSymbol)
	}
	(*cs)[name][class] = sym
	return sym
}
