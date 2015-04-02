# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOFILES=\
	export.go\
	metrics/metric.go\
	mtail.go\
	vm/loader.go\
	tailer/tail.go\
	vm/ast.go\
	vm/compiler.go\
	vm/lexer.go\
	vm/parser.go\
	vm/symtab.go\
	vm/unparser.go\
	vm/vm.go\
	watcher/fake_watcher.go\
	watcher/log_watcher.go\
	watcher/watcher.go\

GOTESTFILES=\
	ex_test.go\
	export_test.go\
	mtail_test.go\
	tailer/tail_test.go\
	vm/lexer_test.go\
	vm/parser_test.go\
	vm/loader_test.go\
	vm/vm_test.go\
	watcher/fake_watcher_test.go\
	watcher/log_watcher_test.go\


CLEANFILES+=\
	vm/parser.go\
	vm/y.output\

all: mtail

mtail: $(GOFILES)
	go build

vm/parser.go: vm/parser.y
	cd vm && go generate

emgen/emgen: emgen/emgen.go
	cd emgen && go build

.PHONY: test
test: $(GOFILES) $(GOTESTFILES)
	go test -v ./...

.PHONY: testrace
testrace: $(GOFILES) $(GOTESTFILES)
	go test -v -race ./...

.PHONY: smoke
smoke: $(GOFILES) $(GOTESTFILES)
	go test -v -test.short ./...

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX ./...

.PHONY: recbench
recbench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX --record_benchmark ./...

.PHONY: testall
testall: testrace bench
