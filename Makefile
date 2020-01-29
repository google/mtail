# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

export GO111MODULE ?= auto
# Build these.
TARGETS = mtail mgen mdot mfmt

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
timeout := 10m
ifeq ($(TRAVIS),true)
timeout := 20m
endif
ifeq ($(CIRCLECI),true)
timeout := 20m
endif
# Let the benchmarks run for a long time.  The timeout is for the total time of
# all benchmarks, not per bench.
benchtimeout := 20m

# Be verbose with `go get`, if UPDATE is y then also update dependencies.
GOGETFLAGS = -v
ifeq ($(UPDATE),y)
GOGETFLAGS += -u
endif

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
GOBIN ?= $(firstword $(subst :, ,$(GOPATH)))/bin

TOGO = $(GOBIN)/togo
$(TOGO):
	go get $(GOGETFLAGS) github.com/flazz/togo

GOYACC = $(GOBIN)/goyacc
$(GOYACC):
	go get $(GOGETFLAGS) golang.org/x/tools/cmd/goyacc

GOFUZZBUILD = $(GOBIN)/go-fuzz-build
$(GOFUZZBUILD):
	go get $(GOGETFLAGS) github.com/dvyukov/go-fuzz/go-fuzz-build

GOFUZZ = $(GOBIN)/go-fuzz
$(GOFUZZ):
	go get $(GOGETFLAGS) github.com/dvyukov/go-fuzz/go-fuzz

GOVERALLS = $(GOBIN)/goveralls
$(GOVERALLS):
	go get $(GOGETFLAGS) github.com/mattn/goveralls

GOX = $(GOBIN)/gox
$(GOX):
	go get github.com/mitchellh/gox


.PHONY: clean covclean crossclean depclean
clean: covclean crossclean
	rm -f $(CLEANFILES)
covclean:
	rm -f *.coverprofile coverage.html $(COVERPROFILES)
crossclean:
	rm -rf build
depclean:
	rm -f .d/*  .*dep-stamp

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

# Very specific static pattern rule to only do this for commandline targets.
# Each commandline must be in a 'main.go' in their respective directory.  The
# MAKEDEPEND rule generates a list of dependencies for the next make run -- the
# first time the rule executes because the target doesn't exist, subsequent
# runs can read the dependencies and update iff they change.
$(TARGETS): %: cmd/%/main.go $(DEPDIR)/%.d print-version |.dep-stamp
	$(MAKEDEPEND)
	go build -ldflags "$(GO_LDFLAGS)" -o $@ $<

internal/vm/parser/parser.go: internal/vm/parser/parser.y | $(GOYACC)
	go generate -x ./$(@D)

internal/mtail/logo.ico: logo.png
	/usr/bin/convert $< -define icon:auto-resize=64,48,32,16 $@ || touch $@

internal/mtail/logo.ico.go: | internal/mtail/logo.ico $(TOGO)
	$(TOGO) -pkg mtail -name logoFavicon -input internal/mtail/logo.ico


###
## Emit the current toolchain version at the start of every goal, if that goal depends on this.
#
.PHONY: print-version
print-version:
	@go version

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


GOX_OSARCH ?= "linux/amd64 windows/amd64 darwin/amd64"
#GOX_OSARCH := ""

.PHONY: crossbuild
crossbuild: $(GOFILES) $(GOGENFILES) print-version | $(GOX) .dep-stamp
	mkdir -p build
	gox --output="./build/mtail_${release}_{{.OS}}_{{.Arch}}" -osarch=$(GOX_OSARCH) -ldflags "$(GO_LDFLAGS)" ./cmd/mtail

.PHONY: test check
check test: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) print-version | $(LOGO_GO) .dep-stamp
	go test -timeout 10s ./...

.PHONY: testrace
testrace: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) print-version | $(LOGO_GO) .dep-stamp
	go test -timeout ${timeout} -race -v -tags=integration ./...

.PHONY: smoke
smoke: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) print-version | .dep-stamp
	go test -timeout 1s -test.short ./...

.PHONY: bench
bench: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) print-version | .dep-stamp
	go test -tags=integration -bench=. -timeout=${benchtimeout} -benchtime=5s -run=BenchmarkProgram ./...

.PHONY: bench_cpu
bench_cpu: print-version | .dep-stamp
	go test -tags=integration -bench=. -run=BenchmarkProgram -timeout=${benchtimeout} -benchtime=5s -cpuprofile=cpu.out internal/mtail/examples_integration_test.go
.PHONY: bench_mem
bench_mem: print-version | .dep-stamp
	go test -tags=integration -bench=. -run=BenchmarkProgram -timeout=${benchtimeout} -benchtime=5s -memprofile=mem.out internal/mtail/examples_integration_test.go

.PHONY: recbench
recbench: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) print-version | .dep-stamp
	go test -bench=. -run=XXX --record_benchmark ./...

.PHONY: regtest
regtest: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) print-version | .dep-stamp
	go test -v -tags=integration -timeout=${timeout} ./...

PACKAGES := $(shell go list -f '{{.Dir}}' ./... | grep -v /vendor/ | grep -v /cmd/ | sed -e "s@$$(pwd)@.@")

.PHONY: testall
testall: testrace bench regtest

IMPORTS := $(shell go list -f '{{join .Imports "\n"}}' ./... | sort | uniq | grep -v mtail)
TESTIMPORTS := $(shell go list -f '{{join .TestImports "\n"}}' ./... | sort | uniq | grep -v mtail)

## make u a container
.PHONY: container
container: Dockerfile
	docker build -t mtail \
		--build-arg version=${version} \
	    --build-arg commit_hash=${revision} \
	    --build-arg build_date=$(shell date -Iseconds --utc) \
	    .

# Append the bin subdirs of every element of the GOPATH list to PATH, so we can find goyacc.
empty :=
space := $(empty) $(empty)
export PATH := $(PATH):$(subst $(space),:,$(patsubst %,%/bin,$(subst :, ,$(GOPATH))))

###
## Fuzz testing
#

# These flags set compatibility with OSS-Fuzz
CXX ?= clang-9
CXXFLAGS ?=
LIB_FUZZING_ENGINE ?= -libfuzzer
OUT ?= .

$(OUT)/vm-fuzzer: $(GOFILES) | $(GOFUZZBUILD)
	$(GOFUZZBUILD) -libfuzzer -o fuzzer.a ./internal/vm
	$(CXX) $(CXXFLAGS) $(LIB_FUZZING_ENGINE) fuzzer.a -lpthread -o $(OUT)/vm-fuzzer

$(OUT)/vm-fuzzer.dict: mgen
	./mgen --dictionary | sort > $@

$(OUT)/vm-fuzzer_seed_corpus.zip: $(wildcard examples/*.mtail)
	zip -j $@ $^


.PHONY: fuzz
fuzz: $(OUT)/vm-fuzzer_seed_corpus.zip $(OUT)/vm-fuzzer $(OUT)/vm-fuzzer.dict
	mkdir -p CORPUS SEED
	unzip -o -d SEED $(OUT)/vm-fuzzer_seed_corpus.zip
	$(OUT)/vm-fuzzer -dict=$(OUT)/vm-fuzzer.dict CORPUS SEE

# make fuzz-min CRASH=example crash
.PHONY: fuzz-min
fuzz-min: $(OUT)/vm-fuzzer $(OUT)/vm-fuzzer.dict
	$(OUT)/vm-fuzzer -dict=$(OUT)/vm-fuzzer.dict -minimize_crash=1 -runs=10000 $(CRASH)

###
## dependency section
#
.PHONY: install_deps
install_deps: .dep-stamp
.dep-stamp: $(GOGENFILES) print-version
	@echo "Install all dependencies, ensuring they're updated"
ifeq ($(GO111MODULE),on)
	go get $(GOGETFLAGS) -t ./...
else
	go get $(GOGETFLAGS) $(IMPORTS)
	go get $(GOGETFLAGS) $(TESTIMPORTS)
endif
	touch $@


###
## Coverage
#
.PHONY: coverage covrep

coverage: coverprofile

coverprofile: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) print-version | $(LOGO_GO) .dep-stamp
	go test -v -covermode=count -coverprofile=$@ -tags=integration -timeout=${timeout} $(PACKAGES)

coverage.html: coverprofile print-version
	go tool cover -html=$< -o $@

covrep: coverage.html
	xdg-open $<

ifeq ($(CIRCLECI),true)
  COVERALLS_SERVICE := circle-ci
endif
ifeq ($(TRAVIS),true)
  COVERALLS_SERVICE := travis-ci
endif

.PHONY: upload_to_coveralls
upload_to_coveralls: | coverprofile $(GOVERALLS)
	$(GOVERALLS) -coverprofile=coverprofile -service=$(COVERALLS_SERVICE)


###
## CircleCI development targets
#

.PHONY: circleci-validate
circleci-validate: .circleci/config.yml
	circleci config validate

# Override this on the make command to say which job to run
CIRCLEJOB ?= fuzzing
.PHONY: circleci-execute
.INTERMEDIATE: tmpconfig.yml
circleci-execute: .circleci/config.yml circleci-validate
ifeq ($(CIRCLECI),true)
	$(error "Don't run this target from within CircleCI!")
endif
	circleci config process $< > tmpconfig.yml
	circleci local execute -c tmpconfig.yml --job $(CIRCLEJOB)
