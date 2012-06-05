# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOFILES=\
	ast.go\
	lexer.go\
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

all: emtail

emtail: parser.go $(GOFILES)
	go build

parser.go: parser.y
	go tool yacc -v y.output -o $@ -p Emtail $<

.PHONY: test
test: parser.go $(GOFILES) $(GOTESTFILES)
	go test -test.v=true

.PHONY: testshort
testshort:
	go test -test.short
