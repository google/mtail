// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package symbol

import (
	"bytes"
	"fmt"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/types"
)

// Kind enumerates the kind of a Symbol.
type Kind int

// Kind enumerates the kinds of symbols found in the program text.
const (
	VarSymbol     Kind = iota // Variables
	CaprefSymbol              // Capture group references
	DecoSymbol                // Decorators
	PatternSymbol             // Named pattern constants
	endSymbol                 // for testing
)

func (k Kind) String() string {
	switch k {
	case VarSymbol:
		return "variable"
	case CaprefSymbol:
		return "capture group reference"
	case DecoSymbol:
		return "decorator"
	case PatternSymbol:
		return "named pattern constant"
	default:
		panic("unexpected symbolkind")
	}
}

// Symbol describes a named program object.
type Symbol struct {
	Name    string             // identifier name
	Kind    Kind               // kind of program object
	Type    types.Type         // object's type
	Pos     *position.Position // Source file position of definition
	Binding interface{}        // binding to storage allocated in runtime
	Addr    int                // Address offset in another structure, object specific
	Used    bool               // Optional marker that this symbol is used after declaration.
}

// NewSymbol creates a record of a given symbol kind, named name, found at loc.
func NewSymbol(name string, kind Kind, pos *position.Position) (sym *Symbol) {
	return &Symbol{name, kind, types.Undef, pos, nil, 0, false}
}

// Scope maintains a record of the identifiers declared in the current program
// scope, and a link to the parent scope.
type Scope struct {
	Parent  *Scope
	Symbols map[string]*Symbol
}

// NewScope creates a new scope within the parent scope.
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

// InsertAlias attempts to insert a duplicate name for an existing symbol into
// the scope.  If the scope already contains an object alt with the alias, the
// scope is unchanged and the function returns alt.  Otherwise, the symbol is
// inserted and the function returns nil.
func (s *Scope) InsertAlias(sym *Symbol, alias string) (alt *Symbol) {
	if alt := s.Symbols[alias]; alt == nil {
		s.Symbols[alias] = sym
	}
	return
}

// Lookup returns the symbol with the given name if it is found in this or any
// parent scope, otherwise nil.
func (s *Scope) Lookup(name string, kind Kind) *Symbol {
	for scope := s; scope != nil; scope = scope.Parent {
		if sym := scope.Symbols[name]; sym != nil && sym.Kind == kind {
			return sym
		}
	}
	return nil
}

// String prints the current scope and all parents to a string, recursing up to
// the root scope.  This method is only used for debugging.
func (s *Scope) String() string {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "scope %p {", s)
	if s != nil {
		fmt.Fprintln(&buf)
		if len(s.Symbols) > 0 {
			for name, sym := range s.Symbols {
				fmt.Fprintf(&buf, "\t%q: %v %q %v\n", name, sym.Kind, sym.Name, sym.Used)
			}
		}
		if s.Parent != nil {
			fmt.Fprintf(&buf, "%s", s.Parent.String())
		}
	}
	fmt.Fprintf(&buf, "}\n")
	return buf.String()
}

// CopyFrom copies all the symbols from another scope object into this one.
// It recurses up the input scope copying all visible symbols into one.
func (s *Scope) CopyFrom(o *Scope) {
	for _, sym := range o.Symbols {
		s.Insert(sym)
	}
	if o.Parent != nil {
		s.CopyFrom(o.Parent)
	}
}
