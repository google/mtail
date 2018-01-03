# Introduction

mtail is implemented in [Go](http://golang.org).

You will need to install Go 1.7 or higher.

# Details

[Clone](http://github.com/google/mtail) the source from GitHub into your `$GOPATH`.  If you don't have a `$GOPATH`, see the next section.

```
cd $GOPATH/src
go get https://github.com/google/mtail
make
```

## For Go First-Timers

An excellent starting guide for people new to Go entirely is here: https://github.com/alco/gostart

If you want to skip the guide, these two references are short but to the point
on setting up the `$GOPATH` workspace:

* https://github.com/golang/go/wiki/SettingGOPATH
* https://github.com/golang/go/wiki/GOPATH#repository-integration-and-creating-go-gettable-projects

Finally, https://golang.org/doc/code.html is the original Go project
documentation for the philosophy on Go workspaces.

### No Really What's the TLDR

Put `export GOPATH=$HOME/go` in your `~/.profile`.

```
export GOPATH=$HOME/go
mkdir -p $GOPATH/src
```

then back up to the Details above.

## Building

Unlike the recommendation for Go projects, `mtail` uses a `Makefile` to build the source.

Having fetched the source, use `make` from the top of the source tree.  This will install all the dependencies, and then build `mtail`.  This assumes that your Go environment is already set up -- see above for hints on setting it up.

The resulting binary will be in `$GOPATH/bin`.

The unit tests can be run with `make test`, which invokes `go test`.  The slower race-detector tests can be run with `make testrace`.

## Contributing

Please use `gofmt` to format your code before committing.  Emacs' go-mode has a lovely [gofmt-before-save](http://golang.org/misc/emacs/go-mode.el) function.

## Troubleshooting

If `make` gives you the following error:

```
../github.com/google/mtail/vm/lexer.go:28: too many errors
```

Then run `make` in that dependency and run `make` again like such:

```
cd ../github.com/google/mtail
make
cd -
make
```
