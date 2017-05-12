// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"bytes"
	"fmt"
)

type SymbolKind int

// SymbolKind enumerates the kinds of symbols found in the program text.
const (
	VarSymbol    SymbolKind = iota // Variables
	CaprefSymbol                   // Capture group references
	DecoSymbol                     // Decorators

	endSymbol
)

// Symbol describes a named program object.
type Symbol struct {
	Name    string      // identifier name
	Kind    SymbolKind  // kind of program object
	Type    Type        // object's type
	Pos     *position   // Source file position of definition
	Binding interface{} // binding to storage allocated in runtime
	Addr    int         // Address offset in another structure, object specific
}

// NewSymbol creates a record of a given symbol kind, named name, found at loc
func NewSymbol(name string, kind SymbolKind, pos *position) (sym *Symbol) {
	return &Symbol{name, kind, Int, pos, nil, 0}
}

// Scope maintains a record of the identifiers declared in the current program
// scope, and a link to the parent scope.
type Scope struct {
	Parent  *Scope
	Symbols map[string]*Symbol
}

// NewScope creates a new scope within the parent scope
func NewScope(parent *Scope) *Scope {
	return &Scope{parent, make(map[string]*Symbol)}
}

// Insert attempts to insert a symbol into the scope.  If the scope already
// contains an object alt with the same name, the scope is unchanged and the
// function returns alt.  Otherwise the symbol is inserted, and returns nil.
func (s *Scope) Insert(sym *Symbol) (alt *Symbol) {
	if alt = s.Symbols[sym.Name]; alt == nil {
		s.Symbols[sym.Name] = sym
	}
	return
}

// lookup returns the symbol with the given name if it is found in this or any
// parent scope, otherwise nil.
func (s *Scope) Lookup(name string) *Symbol {
	for scope := s; scope != nil; scope = scope.Parent {
		if sym := scope.Symbols[name]; sym != nil {
			return sym
		}
	}
	return nil
}

func (s *Scope) String() string {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "scope %p {", s)
	if s != nil {
		fmt.Fprintln(&buf)
		if len(s.Symbols) > 0 {
			for _, sym := range s.Symbols {
				fmt.Fprintf(&buf, "\t%s %s\n", sym.Kind, sym.Name)
			}
		}
		if s.Parent != nil {
			fmt.Fprintf(&buf, "%s", s.Parent.String())
		}
	}
	fmt.Fprintf(&buf, "}\n")
	return buf.String()
}
