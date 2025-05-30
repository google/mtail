# Building mtail

`mtail` is implemented in [Go](http://golang.org).

You will need to install a recent Go.


## `go get`, quick and easy, no version information.

Fetch, build, and install the binary directly with `go get`

`go install github.com/jaqx0r/mtail/cmd/mtail`

NOTE: If you do it this way, you won't have a supported version of `mtail`.

## The "Right Way"

[Clone](http://github.com/jaqx0r/mtail) the source from GitHub into your `$GOPATH`.  If you don't have a `$GOPATH`, see the next section.

```
git clone https://github.com/jaqx0r/mtail
cd mtail
make test install
```

### Building

`mtail` uses a `Makefile` to build the source.  This ensures the generated code is up to date and that the binary is tagged with release information.

Having fetched the source, use `make` from the top of the source tree.  This will install all the dependencies, and then build `mtail`.  This assumes that your Go environment is already set up -- see above for hints on setting it up.

The resulting binary will be in `$GOPATH/bin`.

The unit tests can be run with `make test`, which invokes `go test`.  The slower race-detector tests can be run with `make testrace`.

### Cross-compilation

`goreleaser` is used to build the binaries available for download on the Releases page.  If you want to build your own locally, fetch goreleaser and update the config file locally if necessary.

## No Go

You can still run `mtail` and its programmes with Docker.

```
docker build -t mtail .
docker run -it --rm mtail --help
```

`mtail` is not much use without programme files or logs to parse, you will need to mount a path containing them into the container, as is done with the `-v` flag in the example below:

```
docker run -it --rm -v examples/linecount.mtail:/progs/linecount.mtail -v /var/log:/logs mtail -logtostderr -one_shot -progs /progs/linecount.mtail -logs /logs/messages.log
```

Or, via Docker Compose, e.g. this `docker-compose.yml` snippet example shows with the `volume:` section:

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

Please read the [test writing](Testing.md#test-writing) section for `mtail` test style guidelines.
