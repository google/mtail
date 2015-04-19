# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOFILES=\
	exporter/export.go\
	exporter/export_collectd.go\
	exporter/export_graphite.go\
	exporter/export_json.go\
	exporter/export_prometheus.go\
	exporter/export_statsd.go\
	main.go\
	metrics/metric.go\
	mtail/mtail.go\
	tailer/tail.go\
	vm/ast.go\
	vm/compiler.go\
	vm/lexer.go\
	vm/loader.go\
	vm/parser.go\
	vm/symtab.go\
	vm/unparser.go\
	vm/vm.go\
	watcher/fake_watcher.go\
	watcher/log_watcher.go\
	watcher/watcher.go\

GOTESTFILES=\
	ex_test.go\
	exporter/export_test.go\
	mtail/mtail_test.go\
	tailer/tail_test.go\
	vm/lexer_test.go\
	vm/parser_test.go\
	vm/vm_test.go\
	watcher/fake_watcher_test.go\
	watcher/log_watcher_test.go\


CLEANFILES+=\
	vm/parser.go\
	vm/y.output\

all: mtail

.PHONY: mtail
mtail: $(GOFILES)
	go install

vm/parser.go: vm/parser.y
	cd vm && go generate

emgen/emgen: emgen/emgen.go
	cd emgen && go build

.PHONY: test
test: $(GOFILES) $(GOTESTFILES)
	go test -v -timeout 60s ./...

.PHONY: testrace
testrace: $(GOFILES) $(GOTESTFILES)
	go test -v -timeout 5m -race ./...

.PHONY: smoke
smoke: $(GOFILES) $(GOTESTFILES)
	go test -v -timeout 10s -test.short ./...

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX ./...

.PHONY: recbench
recbench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -run=XXX --record_benchmark ./...

.PHONY: coverage
coverage: gover.coverprofile
gover.coverprofile: $(GOFILES) $(GOTESTFILES)
	for package in exporter metrics mtail tailer vm watcher; do\
		go test -covermode=count -coverprofile=$$package.coverprofile ./$$package;\
    done
	gover

.PHONY: covrep
covrep: coverage.html
	xdg-open $<
coverage.html: gover.coverprofile
	go tool cover -html=$< -o $@

.PHONY: testall
testall: testrace bench
