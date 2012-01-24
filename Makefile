# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOROOT?=/usr/lib/go

include $(GOROOT)/src/Make.inc

TARG=emtail

GOFILES=\
	ast.go\
	lexer.go\
	main.go\
	metric.go\
	parser.go\
	scope.go\
	tail.go\
	unparser.go\
	vm.go\
	watch.go\

GOTESTFILES=\
	ex_test.go\
	lexer_test.go\
	parser_test.go\
	tail_test.go\
	vm_test.go\
	watch_test.go\

CLEANFILES+=\
	parser.go\
	y.output\

include $(GOROOT)/src/Make.cmd

parser.go: parser.y
	goyacc -o $@ -p Emtail $<

.PHONY: test
test:
	gotest

.PHONY: testshort
testshort:
	gotest -test.short
