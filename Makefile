# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOFILES=\
	ast.go\
	compiler.go\
	mtail.go\
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
	mtail_test.go\
	ex_test.go\
	export_test.go\
	lexer_test.go\
	parser_test.go\
	tail_test.go\
	vm_test.go\


CLEANFILES+=\
	parser.go\
	y.output\

all: mtail

mtail: $(GOFILES)
	go build

parser.go: parser.y
	go generate

emgen/emgen: emgen/emgen.go
	cd emgen && go build

.PHONY: test
test: $(GOFILES) $(GOTESTFILES)
	go test -gcflags '-N' -v -race

.PHONY: smoke
smoke: $(GOFILES) $(GOTESTFILES)
	go test -gcflags '-N' -v -test.short -race

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX

.PHONY: recbench
recbench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX --record_benchmark

.PHONY: testall
testall: test bench
