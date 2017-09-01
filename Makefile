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
	metrics/datum/datum.go\
	metrics/datum/int.go\
	metrics/metric.go\
	metrics/store.go\
	mtail/mtail.go\
	tailer/tail.go\
	vm/ast.go\
	vm/checker.go\
	vm/compiler.go\
	vm/driver.go\
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
	bench_test.go\
	exporter/export_test.go\
	exporter/json_test.go\
	exporter/prometheus_test.go\
	exporter/varz_test.go\
	metrics/datum/int_test.go\
	metrics/metric_test.go\
	metrics/store_test.go\
	mtail/mtail_test.go\
	tailer/tail_test.go\
	testdata/reader.go\
	testdata/reader_test.go\
	vm/checker_test.go\
	vm/codegen_test.go\
	vm/lexer_test.go\
	vm/parser_test.go\
	vm/symtab_test.go\
	vm/types_test.go\
	vm/vm_test.go\
	watcher/fake_watcher_test.go\
	watcher/log_watcher_test.go\


CLEANFILES+=\
	vm/parser.go\
	vm/y.output\

all: mtail

.PHONY: clean
clean:
	rm -f $(CLEANFILES) .*dep-stamp

version := $(shell git describe --tags)
revision := $(shell git rev-parse HEAD)

install mtail: $(GOFILES) .dep-stamp
	go install -ldflags "-X main.Version=${version} -X main.Revision=${revision}"

vm/parser.go: vm/parser.y .gen-dep-stamp
	go generate -x ./vm

emgen/emgen: emgen/emgen.go
	cd emgen && go build

.PHONY: test check 
check test: $(GOFILES) $(GOTESTFILES) .dep-stamp
	go test -v -timeout 10s ./... ./testdata

.PHONY: testrace
testrace: $(GOFILES) $(GOTESTFILES) .dep-stamp
	go test -v -timeout 60s -race ./... ./testdata

.PHONY: smoke
smoke: $(GOFILES) $(GOTESTFILES) .dep-stamp
	go test -v -timeout 1s -test.short ./... ./testdata

.PHONY: bench
bench: $(GOFILES) $(GOTESTFILES) .dep-stamp
	go test -bench=. -timeout=60s -run=XXX ./... ./testdata

.PHONY: bench_cpu
bench_cpu:
	go test -bench=. -run=XXX -timeout=60s -cpuprofile=cpu.out
.PHONY: bench_mem
bench_mem:
	go test -bench=. -run=XXX -timeout=60s -memprofile=mem.out

.PHONY: recbench
recbench: $(GOFILES) $(GOTESTFILES) .dep-stamp
	go test -bench=. -run=XXX --record_benchmark ./... ./testdata

.PHONY: coverage
coverage: gover.coverprofile
gover.coverprofile: $(GOFILES) $(GOTESTFILES) .dep-stamp
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

IMPORTS := $(shell go list -f '{{join .Imports "\n"}}' ./... ./testdata | sort | uniq | grep -v mtail)
TESTIMPORTS := $(shell go list -f '{{join .TestImports "\n"}}' ./... ./testdata | sort | uniq | grep -v mtail)

.dep-stamp:
	# Install all dependencies, ensuring they're updated
	go get -u -v $(IMPORTS)
	go get -u -v $(TESTIMPORTS)
	touch $@

.PHONY: install_gen_deps
install_gen_deps: .gen-dep-stamp

.gen-dep-stamp:
	go get golang.org/x/tools/cmd/goyacc
	touch $@

.PHONY: install_coverage_deps
install_coverage_deps: .cov-dep-stamp

.cov-dep-stamp: install_deps
	go get golang.org/x/tools/cmd/cover
	go get github.com/modocache/gover
	go get github.com/mattn/goveralls
	touch $@
