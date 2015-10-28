# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

GOFILES=\
	exporter/collectd.go\
	exporter/export.go\
	exporter/graphite.go\
	exporter/json.go\
	exporter/prometheus.go\
	exporter/statsd.go\
	exporter/varz.go\
	main.go\
	metrics/datum.go\
	metrics/metric.go\
	metrics/store.go\
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
	exporter/json_test.go\
	exporter/prometheus_test.go\
	exporter/varz_test.go\
	mtail/mtail_test.go\
	tailer/tail_test.go\
	testdata/reader.go\
	testdata/reader_test.go\
	vm/compiler_test.go\
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
mtail: $(GOFILES) install_deps
	go install

vm/parser.go: vm/parser.y
	cd vm && go generate

emgen/emgen: emgen/emgen.go
	cd emgen && go build

.PHONY: test
test: $(GOFILES) $(GOTESTFILES) mtail
	go test -v -timeout 60s ./...

.PHONY: testrace
testrace: $(GOFILES) $(GOTESTFILES) mtail
	go test -v -timeout 5m -race ./...

.PHONY: smoke
smoke: $(GOFILES) $(GOTESTFILES) mtail
	go test -v -timeout 10s -test.short ./...

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES)
	go test -bench=. -timeout 60s -run=XXX ./...

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

.PHONY: install_deps
install_deps: .dep-stamp

.dep-stamp: vm/parser.go
	go get -t -v ./...
	touch $@

.PHONY: install_coverage_deps
install_coverage_deps: .cov-dep-stamp

.cov-dep-stamp: install_deps
	go get golang.org/x/tools/cmd/cover
	go get github.com/modocache/gover
	go get github.com/mattn/goveralls
	touch $@
