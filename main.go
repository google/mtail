// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"strings"

	"github.com/golang/glog"
	"github.com/google/mtail/mtail"

	_ "net/http/pprof"
)

var (
	port  = flag.String("port", "3903", "HTTP port to listen on.")
	logs  = flag.String("logs", "", "List of files to monitor.")
	progs = flag.String("progs", "", "Directory containing programs")

	oneShot      = flag.Bool("one_shot", false, "Run once on a log file, dump json, and exit.")
	compileOnly  = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")
	dumpBytecode = flag.Bool("dump_bytecode", false, "Dump bytecode of programs and exit.")

	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
)

func main() {
	flag.Parse()
	if *progs == "" {
		glog.Fatalf("No mtail program directory specified; use -progs")
	}
	if *logs == "" {
		glog.Fatalf("No logs specified to tail; use -logs")
	}
	var logPathnames []string
	for _, pathname := range strings.Split(*logs, ",") {
		if pathname != "" {
			logPathnames = append(logPathnames, pathname)
		}
	}
	if len(logPathnames) == 0 {
		glog.Fatal("No logs to tail.")
	}
	o := mtail.Options{
		Progs:                *progs,
		Logs:                 logPathnames,
		Port:                 *port,
		OneShot:              *oneShot,
		CompileOnly:          *compileOnly,
		DumpBytecode:         *dumpBytecode,
		SyslogUseCurrentYear: *syslogUseCurrentYear,
	}
	m, err := mtail.New(o)
	if err != nil {
		glog.Fatalf("couldn't start: %s", err)
	}
	m.Run()
}
