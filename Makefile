GOROOT=/usr/lib/go

include $(GOROOT)/src/Make.inc

TARG=emtail

GOTESTFILES=parser_test.go lexer_test.go watch_test.go tail_test.go vm_test.go
GOFILES=lexer.go parser.go ast.go unparser.go asm.go scope.go watch.go tail.go vm.go main.go metric.go

CLEANFILES += parser.go y.output 

include $(GOROOT)/src/Make.cmd

parser.go: parser.y
	goyacc -o $@ -p Emtail $<

test:
	gotest

testshort:
	gotest -test.short
