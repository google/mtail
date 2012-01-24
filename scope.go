// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

type scope struct {
	parent *scope
	symtab map[string]int
}

func (s *scope) lookupSym(name string) (int, bool) {
	r, ok := s.symtab[name]
	if !ok && s.parent != nil {
		return s.parent.lookupSym(name)
	}
	return r, ok
}

func (s *scope) addSym(name string, index int) {
	s.symtab[name] = index
}
