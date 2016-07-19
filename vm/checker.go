// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

// checker holds data for a semantic checker
type checker struct {
	errors ErrorList
}

// Check performs a semantic check of the ast node, and returns a boolean
// indicating OK; if ok is not true, then error is a list of errors found.
func Check(node node) error {
	c := &checker{}
	Walk(c, node)
	if len(c.errors) > 0 {
		return c.errors
	}
	return nil
}

func (c *checker) VisitBefore(node node) Visitor {
	return c
}

func (c *checker) VisitAfter(node node) {
}
