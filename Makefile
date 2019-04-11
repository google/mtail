# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.


# Build these.
TARGETS = mtail mgen mdot

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
BIN = $(GOPATH)/bin

TOGO = $(BIN)/togo
$(TOGO):
	go get $(GOGETFLAGS) github.com/flazz/togo

GOYACC = $(BIN)/goyacc
$(GOYACC):
	go get $(GOGETFLAGS) golang.org/x/tools/cmd/goyacc

GOFUZZBUILD = $(BIN)/go-fuzz-build
$(GOFUZZBUILD):
	go get $(GOGETFLAGS) github.com/dvyukov/go-fuzz/go-fuzz-build

GOFUZZ = $(BIN)/go-fuzz
$(GOFUZZ):
	go get $(GOGETFLAGS) github.com/dvyukov/go-fuzz/go-fuzz

GOVERALLS = $(BIN)/goveralls
$(GOVERALLS):
	go get $(GOGETFLAGS) github.com/mattn/goveralls

GOX = $(BIN)/gox
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
release := $(shell git describe --tags | cut -d"-" -f 1,2)

GO_LDFLAGS := "-X main.Branch=${branch} -X main.Version=${version} -X main.Revision=${revision}"

# Very specific static pattern rule to only do this for commandline targets.
# Each commandline must be in a 'main.go' in their respective directory.  The
# MAKEDEPEND rule generates a list of dependencies for the next make run -- the
# first time the rule executes because the target doesn't exist, subsequent
# runs can read the dependencies and update iff they change.
$(TARGETS): %: cmd/%/main.go $(DEPDIR)/%.d | .dep-stamp
	$(MAKEDEPEND)
	go build -ldflags $(GO_LDFLAGS) -o $@ $<

internal/vm/parser/parser.go: internal/vm/parser/parser.y | $(GOYACC)
	go generate -x ./$(@D)

internal/mtail/logo.ico: logo.png
	/usr/bin/convert $< -define icon:auto-resize=64,48,32,16 $@ || touch $@

internal/mtail/logo.ico.go: | internal/mtail/logo.ico $(TOGO)
	$(TOGO) -pkg mtail -name logoFavicon -input internal/mtail/logo.ico


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
crossbuild: $(GOFILES) $(GOGENFILES) | $(GOX) .dep-stamp
	mkdir -p build
	gox --output="./build/mtail_${release}_{{.OS}}_{{.Arch}}" -osarch=$(GOX_OSARCH) -ldflags $(GO_LDFLAGS) ./cmd/mtail

.PHONY: test check
check test: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | $(LOGO_GO) .dep-stamp
	go test -timeout 10s ./...

.PHONY: testrace
testrace: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | $(LOGO_GO) .dep-stamp
	go test -timeout ${timeout} -race -v -tags=integration ./...

.PHONY: smoke
smoke: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | .dep-stamp
	go test -timeout 1s -test.short ./...

.PHONY: bench
bench: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | .dep-stamp
	go test -tags=integration -bench=. -timeout=${benchtimeout} -run=XXX ./...

.PHONY: bench_cpu
bench_cpu: | .dep-stamp
	go test -tags=integration -bench=. -run=XXX -timeout=${benchtimeout} -cpuprofile=cpu.out
.PHONY: bench_mem
bench_mem: | .dep-stamp
	go test -tags=integration
-bench=. -run=XXX -timeout=${benchtimeout} -memprofile=mem.out

.PHONY: recbench
recbench: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | .dep-stamp
	go test -bench=. -run=XXX --record_benchmark ./...

.PHONY: regtest
regtest: | .dep-stamp
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

vm-fuzz.zip: $(GOFILES) | $(GOFUZZBUILD)
	$(GOFUZZBUILD) github.com/google/mtail/internal/vm

.PHONY: fuzz
fuzz: vm-fuzz.zip | $(GOFUZZ)
#	rm -rf workdir
	mkdir -p workdir/corpus
	cp examples/*.mtail workdir/corpus
	$(GOFUZZ) -bin=vm-fuzz.zip -workdir=workdir

###
## dependency section
#
.PHONY: install_deps
install_deps: .dep-stamp
.dep-stamp: internal/vm/parser/parser.go
	@echo "Install all dependencies, ensuring they're updated"
	go get $(GOGETFLAGS) $(IMPORTS)
	go get $(GOGETFLAGS) $(TESTIMPORTS)
	touch $@


###
## Coverage
#
.PHONY: coverage covrep

coverage: coverprofile

coverprofile: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | $(LOGO_GO) .dep-stamp
	go test -v -covermode=count -coverprofile=$@ -tags=integration -timeout=${timeout} $(PACKAGES)

coverage.html: coverprofile
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
