# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

export GO111MODULE ?= on
# Build these.
TARGETS = mtail mgen mdot mfmt

GO_TEST_FLAGS ?=
BENCH_COUNT ?= 1
BASE_REF ?= master
HEAD_REF ?= $(shell git symbolic-ref HEAD -q --short | tr / - 2>/dev/null)

all: $(TARGETS)

# Install them here
PREFIX ?= usr/local

# Place to store dependencies.
DEPDIR = .d
# Can't use a dependency rule here.
$(shell install -d $(DEPDIR))

# This rule finds all non-standard-library dependencies of each target and emits them to a makefile include.
# Thanks mrtazz: https://unwiredcouch.com/2016/05/31/go-make.html
MAKEDEPEND = echo "$@: $$(go list -f '{{if not .Standard}}{{.Dir}}{{end}}' $$(go list -f '{{ join .Deps "\n" }}' $<) | sed -e 's@$$@/*.go@' | tr "\n" " " )" > $(DEPDIR)/$@.d

# This rule allows the dependencies to not exist yet, for the first run.
$(DEPDIR)/%.d: ;
.PRECIOUS: $(DEPDIR)/%.d

# This instruction loads any dependency includes for our targets.
-include $(patsubst %,$(DEPDIR)/%.d,$(TARGETS))

# Set the timeout for tests run under the race detector.
timeout := 120s
ifeq ($(CI),true)
timeout := 10m
endif
# Let the benchmarks run for a long time.  The timeout is for the total time of
# all benchmarks, not per bench.
benchtimeout := 120m

GOFILES=$(shell find . -name '*.go' -a ! -name '*_test.go')

GOTESTFILES=$(shell find . -name '*_test.go')

GOGENFILES=internal/vm/parser/parser.go\
	internal/mtail/logo.ico.go


CLEANFILES+=\
	internal/vm/parser/parser.go\
	internal/vm/parser/y.output\
	internal/mtail/logo.ico.go\
	internal/mtail/logo.ico\

# A place to install tool dependencies.
GOBIN ?= $(firstword $(subst :, ,$(shell go env GOPATH)))/bin
export PATH := $(GOBIN):$(PATH)

TOGO = $(GOBIN)/togo
$(TOGO):
	go install github.com/flazz/togo@latest

GOYACC = $(GOBIN)/goyacc
$(GOYACC):
	go install golang.org/x/tools/cmd/goyacc@latest

GOFUZZBUILD = $(GOBIN)/go114-fuzz-build
$(GOFUZZBUILD):
	go install github.com/mdempsky/go114-fuzz-build@latest

GOFUZZ = $(GOBIN)/go-fuzz
$(GOFUZZ):
	go install github.com/dvyukov/go-fuzz/go-fuzz@latest

GOTESTSUM = $(GOBIN)/gotestsum
$(GOTESTSUM):
	go install gotest.tools/gotestsum@latest

BENCHSTAT = $(GOBIN)/benchstat
$(BENCHSTAT):
	go install golang.org/x/perf/cmd/benchstat@latest


.PHONY: clean covclean crossclean depclean veryclean
clean: covclean crossclean
	rm -f $(CLEANFILES)
covclean:
	rm -f *.coverprofile coverage.html $(COVERPROFILES)
crossclean:
	rm -rf build
depclean:
	rm -f .d/*  .*dep-stamp
veryclean: clean depclean

.PHONY: lint
lint:
	golangci-lint run ./...

branch := $(shell git rev-parse --abbrev-ref HEAD)
version := $(shell git describe --tags --always --dirty)
revision := $(shell git rev-parse HEAD)
release := $(shell git describe --tags --always --dirty | cut -d"-" -f 1,2)

GO_LDFLAGS := -X main.Branch=${branch} -X main.Version=${version} -X main.Revision=${revision}

ifeq ($(STATIC),y)
# -s Omit symbol table and debug info
# -w Omit DWARF symbol table
# -extldflags -static and CGO_ENABLED=0 to make pure static
GO_LDFLAGS += -w -s -extldflags "-static"
export CGO_ENABLED=0
endif

# Show all errors, not just limit to 10.
GO_GCFLAGS = -e

# Very specific static pattern rule to only do this for commandline targets.
# Each commandline must be in a 'main.go' in their respective directory.  The
# MAKEDEPEND rule generates a list of dependencies for the next make run -- the
# first time the rule executes because the target doesn't exist, subsequent
# runs can read the dependencies and update iff they change.
$(TARGETS): %: cmd/%/main.go $(DEPDIR)/%.d | print-version .dep-stamp
	$(MAKEDEPEND)
	go build -gcflags "$(GO_GCFLAGS)" -ldflags "$(GO_LDFLAGS)" -o $@ $<

internal/vm/parser/parser.go: internal/vm/parser/parser.y | $(GOYACC)
	go generate -x ./$(@D)

internal/mtail/logo.ico: logo.png
	/usr/bin/convert $< -define icon:auto-resize=64,48,32,16 $@ || touch $@

internal/mtail/logo.ico.go: | internal/mtail/logo.ico $(TOGO)
	togo -pkg mtail -name logoFavicon -input internal/mtail/logo.ico


###
## Emit the current toolchain version at the start of every goal, if that goal depends on this.
#
.PHONY: print-version
print-version:
	which go
	go version
	go env

###
## Install rules
#
# Would subst all $(TARGETS) except other binaries are just for development.
INSTALLED_TARGETS = $(PREFIX)/bin/mtail

.PHONY: install
install: $(INSTALLED_TARGETS)

$(PREFIX)/bin/%: %
	install -d $(@D)
	install -m 755 $< $@

.PHONY: test check
check test: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | print-version $(LOGO_GO) .dep-stamp
	go test $(GO_TEST_FLAGS) -gcflags "$(GO_GCFLAGS)" -timeout 10s ./...

.PHONY: testrace
testrace: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | print-version $(LOGO_GO) .dep-stamp
	go test $(GO_TEST_FLAGS) -gcflags "$(GO_GCFLAGS)" -timeout ${timeout} -race -v ./...

.PHONY: smoke
smoke: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | print-version $(LOGO_GO) .dep-stamp
	go test $(GO_TEST_FLAGS) -gcflags "$(GO_GCFLAGS)" -timeout 1s -test.short ./...

.PHONY: regtest
regtest: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | print-version $(LOGO_GO) .dep-stamp
	go test $(GO_TEST_FLAGS) -gcflags "$(GO_GCFLAGS)" -v -timeout=${timeout} ./...

TESTRESULTS ?= test-results
TESTCOVERPROFILE ?= out.coverprofile

.PHONY: junit-regtest
junit-regtest: $(TESTRESULTS)/test-output.xml $(TESTCOVERPROFILE)

$(TESTRESULTS)/test-output.xml $(TESTCOVERPROFILE): $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | print-version .dep-stamp $(GOTESTSUM)
	mkdir -p $(TESTRESULTS)
	gotestsum --debug --junitfile $(TESTRESULTS)/test-output.xml -- $(GO_TEST_FLAGS) -cpu 1,2,4 -race -parallel 1 -coverprofile=$(TESTCOVERPROFILE) --covermode=atomic -v -timeout=${timeout} -gcflags "$(GO_GCFLAGS)" ./...

.PHONY: bench
bench: $(TESTRESULTS)/benchmark-results-$(HEAD_REF).txt

$(TESTRESULTS)/benchmark-results-$(HEAD_REF).txt: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | print-version .dep-stamp
	mkdir -p $(TESTRESULTS)
	go test -cpu 1,2,4 -bench=. -count=$(BENCH_COUNT) -timeout=${benchtimeout} -run=^a ./... | tee $@

.PHONY: benchstat
benchstat: $(TESTRESULTS)/benchstat.txt
$(TESTRESULTS)/benchstat.txt: $(TESTRESULTS)/benchmark-results-$(HEAD_REF).txt | print-version $(BENCHSTAT)
	(test -s $(TESTRESULTS)/benchmark-results-$(BASE_REF).txt && benchstat $(TESTRESULTS)/benchmark-results-$(BASE_REF).txt $< || benchstat $<) | tee $@


PACKAGES := $(shell go list -f '{{.Dir}}' ./... | grep -v /vendor/ | grep -v /cmd/ | sed -e "s@$$(pwd)@.@")

.PHONY: testall
testall: testrace bench regtest

## make u a container
.PHONY: container
container: Dockerfile
	docker build -t mtail \
		--build-arg version=${version} \
	    --build-arg commit_hash=${revision} \
	    --build-arg build_date=$(shell date -Iseconds --utc) \
	    .

###
## Fuzz testing
#

# These flags set compatibility with OSS-Fuzz
CXX ?= clang-9
CXXFLAGS ?=
LIB_FUZZING_ENGINE ?= -fsanitize=fuzzer
OUT ?= .

$(OUT)/vm-fuzzer: $(GOFILES) | $(GOFUZZBUILD)
	go114-fuzz-build -o fuzzer.a ./internal/vm
	$(CXX) $(CXXFLAGS) $(LIB_FUZZING_ENGINE) fuzzer.a -lpthread -o $(OUT)/vm-fuzzer

$(OUT)/vm-fuzzer.dict: mgen
	./mgen --dictionary | sort > $@

$(OUT)/vm-fuzzer_seed_corpus.zip: $(wildcard examples/*.mtail) $(wildcard internal/vm/fuzz/*.mtail)
	zip -j $@ $^

.INTERMEDIATE: SEED/*
SEED: $(OUT)/vm-fuzzer_seed_corpus.zip
	mkdir -p SEED
	unzip -o -d SEED $<

.PHONY: fuzz
fuzz: SEED $(OUT)/vm-fuzzer $(OUT)/vm-fuzzer.dict
	mkdir -p CORPUS
	$(OUT)/vm-fuzzer -dict=$(OUT)/vm-fuzzer.dict CORPUS SEED

.PHONY: fuzz-regtest
fuzz-regtest: $(OUT)/vm-fuzzer SEED
	$(OUT)/vm-fuzzer -rss_limit_mb=4096 $(shell ls SEED/*.mtail)

CRASH ?=

.PHONY: fuzz-repro
fuzz-repro: $(OUT)/vm-fuzzer mtail
	$(OUT)/vm-fuzzer $(CRASH) || true  # Want to continue
	./mtail --logtostderr --vmodule=loader=2,checker=2,types=2 --mtailDebug=3 --dump_ast_types --dump_bytecode --compile_only --progs $(CRASH)

# make fuzz-min CRASH=example crash
.PHONY: fuzz-min
fuzz-min: $(OUT)/vm-fuzzer $(OUT)/vm-fuzzer.dict
	$(OUT)/vm-fuzzer -dict=$(OUT)/vm-fuzzer.dict -minimize_crash=1 -runs=10000 $(CRASH)

###
## dependency section
#
.PHONY: install_deps
install_deps: .dep-stamp
.dep-stamp: | print-version $(GOGENFILES)
	go mod download
	touch $@


###
## Coverage
#
.PHONY: coverage covrep

coverage: coverprofile

coverprofile: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | print-version $(LOGO_GO) .dep-stamp
	go test -v -covermode=count -coverprofile=$@ -timeout=${timeout} $(PACKAGES)

coverage.html: coverprofile | print-version
	go tool cover -html=$< -o $@

covrep: coverage.html
	xdg-open $<

###
## Github issue tracking
#
GHI = $(GOBIN)/ghi
$(GHI):
	go install github.com/markbates/ghi@latest

issue-fetch: | $(GHI)
	ghi fetch

issue-list: | $(GHI)
	ghi list

ISSUE?=1
issue-show: | $(GHI)
	ghi show $(ISSUE)
