# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOFILES=\
	compiler/ast.go\
	compiler/compiler.go\
	mtail.go\
	export.go\
	compiler/lexer.go\
	metrics/metric.go\
	compiler/parser.go\
	progs.go\
	compiler/symtab.go\
	tailer/tail.go\
	compiler/unparser.go\
	vm/vm.go\
	watcher/watcher.go\
	watcher/log_watcher.go\
	watcher/fake_watcher.go\

GOTESTFILES=\
	mtail_test.go\
	ex_test.go\
	export_test.go\
	compiler/lexer_test.go\
	compiler/parser_test.go\
	tailer/tail_test.go\
	compiler/vm_test.go\
	watcher/fake_watcher_test.go\
	watcher/log_watcher_test.go\


CLEANFILES+=\
	compiler/parser.go\
	compiler/y.output\

all: mtail

mtail: $(GOFILES)
	go build

compiler/parser.go: compiler/parser.y
	cd compiler && go generate

emgen/emgen: emgen/emgen.go
	cd emgen && go build

.PHONY: test
test: $(GOFILES) $(GOTESTFILES)
	go test -gcflags '-N -l' -v -race

.PHONY: smoke
smoke: $(GOFILES) $(GOTESTFILES)
	go test -gcflags '-N -l' -v -test.short -race

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX

.PHONY: recbench
recbench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX --record_benchmark

.PHONY: testall
testall: test bench
