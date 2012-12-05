# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOFILES=\
	ast.go\
	compiler.go\
	emtail.go\
	export.go\
	lexer.go\
	metric.go\
	parser.go\
	symtab.go\
	tail.go\
	unparser.go\
	vm.go\

GOTESTFILES=\
	emtail_test.go\
	ex_test.go\
	export_test.go\
	lexer_test.go\
	parser_test.go\
	tail_test.go\
	vm_test.go\


CLEANFILES+=\
	parser.go\
	y.output\

all: emtail

emtail: $(GOFILES)
	go build

parser.go: parser.y
	go tool yacc -v y.output -o $@ -p Emtail $<

.PHONY: test
test: $(GOFILES) $(GOTESTFILES)
	go test -gcflags '-N' -test.v=true

.PHONY: testshort
testshort: $(GOFILES) $(GOTESTFILES)
	go test -test.short

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES)
	go test -test.bench=.*

.PHONY: testall
testall: test bench
