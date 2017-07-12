// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"

	"github.com/golang/glog"
	"github.com/google/mtail/mtail"

	_ "net/http/pprof"
)

type seqStringFlag []string

func (f *seqStringFlag) String() string {
	return fmt.Sprint(*f)
}

func (f *seqStringFlag) Set(value string) error {
	for _, v := range strings.Split(value, ",") {
		*f = append(*f, v)
	}
	return nil
}

var logs seqStringFlag

var (
	port   = flag.String("port", "3903", "HTTP port to listen on.")
	logFds = flag.String("logfds", "", "List of file descriptor numbers to monitor, comma separated.")
	progs  = flag.String("progs", "", "Name of the directory containing mtail programs")

	oneShot        = flag.Bool("one_shot", false, "Run the contents of the provided logs until EOF and exit.")
	oneShotMetrics = flag.Bool("one_shot_metrics", false, "Dump metrics (to stdout) after one shot mode.")
	compileOnly    = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")
	dumpAst        = flag.Bool("dump_ast", false, "Dump AST of programs after parse (to INFO log).")
	dumpAstTypes   = flag.Bool("dump_ast_types", false, "Dump AST of programs with type annotation after typecheck (to INFO log).")
	dumpBytecode   = flag.Bool("dump_bytecode", false, "Dump bytecode of programs (to INFO log).")

	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
)

func init() {
	flag.Var(&logs, "logs", "List of log files to monitor, separated by commas.  This flag may be specified multiple times.")
}

var (
	// Externally supplied by the linker
	Version   string
	Revision  string
	GoVersion = runtime.Version()
)

func buildInfo() string {
	return fmt.Sprintf("mtail version %s git revision %s go version %s", Version, Revision, GoVersion)
}

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "%s\n", buildInfo())
		fmt.Fprintf(os.Stderr, "\nUsage:\n")
		flag.PrintDefaults()
	}
	flag.Parse()
	glog.Info(buildInfo())
	if *progs == "" {
		glog.Exitf("No mtail program directory specified; use -progs")
	}
	var logPathnames []string
	var logDescriptors []int
	if !(*dumpBytecode || *dumpAst || *dumpAstTypes || *compileOnly) {
		if len(logs) == 0 && *logFds == "" {
			glog.Exitf("No logs specified to tail; use -logs or -logfds")
		}
		for _, pathname := range logs {
			if pathname != "" {
				logPathnames = append(logPathnames, pathname)
			}
		}
		for _, fdStr := range strings.Split(*logFds, ",") {
			fdNum, err := strconv.Atoi(fdStr)
			if err == nil {
				logDescriptors = append(logDescriptors, fdNum)
			}
		}
		if len(logPathnames) == 0 && len(logDescriptors) == 0 {
			glog.Exit("No logs to tail.")
		}
	}
	o := mtail.Options{
		Progs:                *progs,
		LogPaths:             logPathnames,
		LogFds:               logDescriptors,
		Port:                 *port,
		OneShot:              *oneShot,
		OneShotMetrics:       *oneShotMetrics,
		CompileOnly:          *compileOnly,
		DumpAst:              *dumpAst,
		DumpAstTypes:         *dumpAstTypes,
		DumpBytecode:         *dumpBytecode,
		SyslogUseCurrentYear: *syslogUseCurrentYear,
		BuildInfo:            buildInfo(),
	}
	m, err := mtail.New(o)
	if err != nil {
		glog.Fatalf("couldn't start: %s", err)
	}
	m.Run()
}
