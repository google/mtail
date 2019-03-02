# Building mtail

mtail is implemented in [Go](http://golang.org).

You will need to install Go 1.7 or higher.

## Go

[Clone](http://github.com/google/mtail) the source from GitHub into your `$GOPATH`.  If you don't have a `$GOPATH`, see the next section.

```
go get -u github.com/google/mtail
cd $GOPATH/src/github.com/google/mtail
make
```

### For Go First-Timers

An excellent starting guide for people new to Go entirely is here: https://github.com/alco/gostart

If you want to skip the guide, these two references are short but to the point
on setting up the `$GOPATH` workspace:

* https://github.com/golang/go/wiki/SettingGOPATH
* https://github.com/golang/go/wiki/GOPATH#repository-integration-and-creating-go-gettable-projects

Finally, https://golang.org/doc/code.html is the original Go project
documentation for the philosophy on Go workspaces.

#### No Really, What is the TLDR

Put `export GOPATH=$HOME/go` in your `~/.profile`.

```
export GOPATH=$HOME/go
mkdir -p $GOPATH/src
```

then back up to the Details above.

### Building

Unlike the recommendation for Go projects, `mtail` uses a `Makefile` to build the source.  This ensures the generated code is up to date and that the binary is tagged with release information.

(You dont have to use the Makefile and can try `go install github.com/google/mtail/cmd/mtail` directly, but if you have problems you'll be asked for the release information.)

Having fetched the source, use `make` from the top of the source tree.  This will install all the dependencies, and then build `mtail`.  This assumes that your Go environment is already set up -- see above for hints on setting it up.

The resulting binary will be in `$GOPATH/bin`.

The unit tests can be run with `make test`, which invokes `go test`.  The slower race-detector tests can be run with `make testrace`.

### Cross-compilation

The `Makefile` has a `crossbuild` target for building on different platforms.  By default it builds for a few `amd64` targets:

```
% make crossbuild
mkdir -p build
gox --output="./build/mtail_v3.0.0-rc10_{{.OS}}_{{.Arch}}" -osarch="linux/amd64 windows/amd64 darwin/amd64" -ldflags "-X main.Version=v3.0.0-rc10-72-gcbea8a8 -X main.Revision=cbea8a810942be1129d58c37b27a55987a384776"
Number of parallel builds: 3

-->     linux/amd64: github.com/google/mtail
-->    darwin/amd64: github.com/google/mtail
-->   windows/amd64: github.com/google/mtail
```

but you can override it with the environment variable `GOX_OSARCH` like so:

```
% make GOX_OSARCX=linux/arm crossbuild
mkdir -p build
gox --output="./build/mtail_v3.0.0-rc10_{{.OS}}_{{.Arch}}" -osarch="linux/amd64 windows/amd64 darwin/amd64" -ldflags "-X main.Version=v3.0.0-rc10-72-gcbea8a8 -X main.Revision=cbea8a810942be1129d58c37b27a55987a384776"
Number of parallel builds: 3

-->    darwin/amd64: github.com/google/mtail
-->   windows/amd64: github.com/google/mtail
-->     linux/amd64: github.com/google/mtail
```

## No Go

You can still build and develop **mtail** with Docker.

```
docker build -t mtail .
docker run -it --rm mtail --help
```

**mtail** is not much use without a configuration file or logs to parse, you will need to mount a path containing them into the container, like so:

```
docker run -it --rm -v examples/linecount.mtail:/progs/linecount.mtail -v /var/log:/logs mtail -logtostderr -one_shot -progs /progs/linecount.mtail -logs /logs/messages.log
```

Or, via Docker Compose, e.g. this `docker-compose.yml` snippet example:

```yaml
service:
  mtail:
    image: mtail
    command:
      - -logtostderr
      - -one_shot
      - -progs
      - /progs/linecount.mtail
      - -logs
      - /logs/messages.log
    volume:
      - type: bind
        source: /var/log
        target: /logs
        readonly: true
      - type: bind
        source: examples/linecount.mtail
        target: /progs/linecount.mtail
```

## Contributing

Please use `gofmt` to format your code before committing.  Emacs' go-mode has a lovely [gofmt-before-save](http://golang.org/misc/emacs/go-mode.el) function.

## Troubleshooting

If `make` gives you the following error:

```
../github.com/google/mtail/vm/lexer.go:28: too many errors
```

Then run `make` in that dependency and run `make` again like so:

```
cd ../github.com/google/mtail
make
cd -
make
```
