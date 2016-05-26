# Introduction

mtail is implemented in [Go](http://golang.org).

You will need to install Go 1.4 or higher.

# Details

[Clone](http://github.com/google/mtail) the source from GitHub.

mtail uses `make` to build.  Type `make` at the commandline to install all the dependencies, and then build mtail.  This assumes that your Go environment is already set up.

Run the unit tests with `make test`, which invokes `gotest`.

The resulting binary will be in `$GOPATH/bin`.

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