// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"strconv"
	"strings"

	"github.com/golang/glog"
	"github.com/google/mtail/mtail"

	_ "net/http/pprof"
)

var (
	port   = flag.String("port", "3903", "HTTP port to listen on.")
	logs   = flag.String("logs", "", "List of files to monitor.")
	logFds = flag.String("logfds", "", "List of file descriptors to monitor.")
	progs  = flag.String("progs", "", "Directory containing programs")

	oneShot        = flag.Bool("one_shot", false, "Run on logs until EOF and exit.")
	oneShotMetrics = flag.Bool("one_shot_metrics", false, "Dump metrics to stdout after one shot mode.")
	compileOnly    = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")
	dumpBytecode   = flag.Bool("dump_bytecode", false, "Dump bytecode of programs and exit.")

	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
)

func main() {
	flag.Parse()
	if *progs == "" {
		glog.Exitf("No mtail program directory specified; use -progs")
	}
	if *logs == "" && *logFds == "" {
		glog.Exitf("No logs specified to tail; use -logs or -logfds")
	}
	var logPathnames []string
	for _, pathname := range strings.Split(*logs, ",") {
		if pathname != "" {
			logPathnames = append(logPathnames, pathname)
		}
	}
	var logDescriptors []int
	for _, fdStr := range strings.Split(*logFds, ",") {
		fdNum, err := strconv.Atoi(fdStr)
		if err == nil {
			logDescriptors = append(logDescriptors, fdNum)
		}
	}
	if len(logPathnames) == 0 && len(logDescriptors) == 0 {
		glog.Exit("No logs to tail.")
	}
	o := mtail.Options{
		Progs:                *progs,
		LogPaths:             logPathnames,
		LogFds:               logDescriptors,
		Port:                 *port,
		OneShot:              *oneShot,
		OneShotMetrics:       *oneShotMetrics,
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
