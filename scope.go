// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

type scope struct {
        parent  *scope
        symtab  map[string]int
}

func (s *scope) lookupSym(name string) bool {
        _, ok := s.symtab[name]
        if !ok && s.parent != nil {
                return s.parent.lookupSym(name)
        }
        return ok
}

func (s *scope) addSym(name string) {
        s.symtab[name] = 1
}
