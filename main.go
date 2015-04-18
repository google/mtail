// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"

	"github.com/google/mtail/mtail"

	_ "net/http/pprof"
)

var (
	port  = flag.String("port", "3903", "HTTP port to listen on.")
	logs  = flag.String("logs", "", "List of files to monitor.")
	progs = flag.String("progs", "", "Directory containing programs")

	oneShot = flag.Bool("one_shot", false, "Run once on a log file, dump json, and exit.")

	compileOnly = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")

	dumpBytecode = flag.Bool("dump_bytecode", false, "Dump bytecode of programs and exit.")

	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
)

func main() {
	flag.Parse()
	o := mtail.Options{*progs, *logs, *port, *oneShot, *compileOnly, *dumpBytecode, *syslogUseCurrentYear}
	m := mtail.New(o)
	m.Run()
}
