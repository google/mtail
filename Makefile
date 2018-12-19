# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

DEPDIR = .d
$(shell install -d $(DEPDIR))
MAKEDEPEND = echo "$@: $$(go list -f '{{if not .Standard}}{{.Dir}}{{end}}' $$(go list -f '{{ join .Deps "\n" }}' $<) | sed -e 's@$$@/*.go@' | tr "\n" " " )" > $(DEPDIR)/$@.d

$(DEPDIR)/%.d: ;
.PRECIOUS: $(DEPDIR)/%.d

-include $(patsubst %,$(DEPDIR)/%.d,$(TARGETS))

# Set the timeout for tests run under the race detector.
timeout := 60s
ifeq ($(TRAVIS),true)
timeout := 5m
endif
ifeq ($(CIRCLECI),true)
timeout := 5m
endif
# Let the benchmarks run for a long time.  The timeout is for the total time of
# all benchmarks, not per bench.
benchtimeout := 20m

GOGETFLAGS="-v"
ifeq ($(UPGRADE),"y")
GOGETFLAGS=$(GOGETFLAGS) "-u"
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

all: mtail

.PHONY: clean covclean crossclean
clean: covclean crossclean
	rm -f $(CLEANFILES) .*dep-stamp
covclean:
	rm -f *.coverprofile coverage.html
crossclean:
	rm -rf build

version := $(shell git describe --tags --always --dirty)
revision := $(shell git rev-parse HEAD)
release := $(shell git describe --tags | cut -d"-" -f 1,2)

GO_LDFLAGS := "-X main.Version=${version} -X main.Revision=${revision}"

.PHONY: install
install: $(GOFILES) $(GOGENFILES)
	go install -ldflags $(GO_LDFLAGS) ./cmd/mtail

mtail: cmd/mtail/mtail.go $(DEPDIR)/mtail.d
	$(MAKEDEPEND)
	go build -ldflags $(GO_LDFLAGS) -o $@ $<

internal/vm/parser/parser.go: internal/vm/parser/parser.y | .gen-dep-stamp
	go generate -x ./$(@D)

internal/mtail/logo.ico: logo.png
	/usr/bin/convert $< -define icon:auto-resize=64,48,32,16 $@ || touch $@

internal/mtail/logo.ico.go: internal/mtail/logo.ico | .gen-dep-stamp
	go run github.com/flazz/togo -pkg mtail -name logoFavicon -input $<

emgen/emgen: emgen/emgen.go
	cd emgen && go build


GOX_OSARCH ?= "linux/amd64 windows/amd64 darwin/amd64"
#GOX_OSARCH := ""

.PHONY: crossbuild
crossbuild: $(GOFILES) $(GOGENFILES) | .dep-stamp .crossbuild-dep-stamp
	mkdir -p build
	gox --output="./build/mtail_${release}_{{.OS}}_{{.Arch}}" -osarch=$(GOX_OSARCH) -ldflags $(GO_LDFLAGS)

.PHONY: test check
check test: $(GOFILES) $(GOGENFILES) | $(LOGO_GO) .dep-stamp
	go test -timeout 10s ./...

.PHONY: testrace
testrace: $(GOFILES) $(GOGENFILES) | $(LOGO_GO) .dep-stamp
	go test -timeout ${timeout} -race -v ./...

.PHONY: testex
testex: | .dep-stamp
	go test -timeout ${timeout} -run Test.*ExamplePrograms -v

.PHONY: smoke
smoke: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | .dep-stamp
	go test -timeout 1s -test.short ./...

.PHONY: ex_test
ex_test: ex_test.go testdata/* examples/* | .dep-stamp
	go test -run TestExamplePrograms --logtostderr

.PHONY: bench
bench: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | .dep-stamp
	go test -bench=. -timeout=${benchtimeout} -run=XXX ./...

.PHONY: bench_cpu
bench_cpu: | .dep-stamp
	go test -bench=. -run=XXX -timeout=${benchtimeout} -cpuprofile=cpu.out
.PHONY: bench_mem
bench_mem: | .dep-stamp
	go test -bench=. -run=XXX -timeout=${benchtimeout} -memprofile=mem.out

.PHONY: recbench
recbench: $(GOFILES) $(GOGENFILES) $(GOTESTFILES) | .dep-stamp
	go test -bench=. -run=XXX --record_benchmark ./...

.PHONY: regtest
regtest: | .dep-stamp
	tests/regtest.sh

PACKAGES := $(shell find . -name '*.go' -exec dirname {} \; | sort -u)

.PHONY: testall
testall: testrace bench regtest

IMPORTS := $(shell go list -f '{{join .Imports "\n"}}' ./... | sort | uniq | grep -v mtail)
TESTIMPORTS := $(shell go list -f '{{join .TestImports "\n"}}' ./... | sort | uniq | grep -v mtail)

ifeq ($(CIRCLECI),true)
  COVERALLS_SERVICE := circle-ci
endif
ifeq ($(TRAVIS),true)
  COVERALLS_SERVICE := travis-ci
endif

upload_to_coveralls: gover.coverprofile
	goveralls -coverprofile=gover.coverprofile -service=$(COVERALLS_SERVICE)

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
vm-fuzz.zip: $(GOFILES) | .fuzz-dep-stamp
	go-fuzz-build github.com/google/mtail/internal/vm

.PHONY: fuzz
fuzz: vm-fuzz.zip
#	rm -rf workdir
	mkdir -p workdir/corpus
	cp examples/*.mtail workdir/corpus
	go-fuzz -bin=vm-fuzz.zip -workdir=workdir

###
## dependency section
#
.PHONY: install_deps
install_deps: .dep-stamp
.dep-stamp: internal/vm/parser/parser.go
	@echo "Install all dependencies, ensuring they're updated"
	go get $(UPGRADE) -v $(IMPORTS)
	go get $(UPGRADE) -v $(TESTIMPORTS)
	touch $@

.PHONY: install_gen_deps
install_gen_deps: .gen-dep-stamp
.gen-dep-stamp:
	go get $(UPGRADE) -v golang.org/x/tools/cmd/goyacc
	go get $(UPGRADE) -v github.com/flazz/togo
	touch $@

.PHONY: install_coverage_deps
install_coverage_deps: .cov-dep-stamp
.cov-dep-stamp:
	go get $(UPGRADE) -v github.com/mattn/goveralls
	touch $@

.PHONY: install_crossbuild
install_crossbuild: .crossbuild-dep-stamp
.crossbuild-dep-stamp:
	go get github.com/mitchellh/gox
	touch $@

.PHONY: install-fuzz-deps
install-fuzz-deps: .fuzz-dep-stamp
.fuzz-dep-stamp:
	go get $(UPGRADE) -v github.com/dvyukov/go-fuzz/go-fuzz
	go get $(UPGRADE) -v github.com/dvyukov/go-fuzz/go-fuzz-build
	touch $@
###
## Coverage
#
COVERPROFILES := $(patsubst %,%/.coverprofile,$(PACKAGES))

.PHONY: coverage covrep
coverage: gover.coverprofile
covrep: coverage.html
	xdg-open $<

# Coverage is just concatenated together, stripping out the 'mode' header from each copy.
gover.coverprofile: $(COVERPROFILES)
	echo "mode: count" > $@
	grep -h -v "mode: " $^ >> $@

coverage.html: gover.coverprofile | .cov-dep-stamp
	go tool cover -html=$< -o $@

# Coverage profiles per package depend on all the source files in that package,
# so we need secondary expansion so that the wildcard rule is expanded at the
# correct time.  Put at the end of the Makefile as it turns it on for all rules
# after this point.
.SECONDEXPANSION:
$(COVERPROFILES): %.coverprofile: $$(wildcard %*.go)
	go test -covermode=count -coverprofile=$@ ./$$(dirname $@)
