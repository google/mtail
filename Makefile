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
	progs.go\
	symtab.go\
	tail.go\
	unparser.go\
	vm.go\
	watcher.go\

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

emgen/emgen: emgen/emgen.go
	cd emgen && go build

.PHONY: test
test: $(GOFILES) $(GOTESTFILES)
	go test -gcflags '-N' -test.v=true

.PHONY: smoke
smoke: $(GOFILES) $(GOTESTFILES)
	go test -gcflags '-N' -test.v=true -test.short

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES)
	go test -test.bench=.*

.PHONY: recbench
recbench: $(GOFILES) $(GOTESTFILES)
	go test -test.bench=.* --record_benchmark

.PHONY: testall
testall: test bench
