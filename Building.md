# Introduction

mtail is implemented in [Go](http://golang.org).

You will need to install the golang-tip package for your distribution.  The Ubuntu PPA is at https://launchpad.net/~gophers/+archive/go

# Details

[Checkout](http://code.google.com/p/mtail/source/checkout) the source from Git.

mtail uses `make` to build.

Run the unit tests with `make test`, which invokes `gotest`.

Please use `gofmt` to format your code before committing.  Emacs' go-mode has a lovely [gofmt-before-save](http://golang.org/misc/emacs/go-mode.el) function.