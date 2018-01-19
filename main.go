// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"fmt"
	"net"
	"os"
	"runtime"
	"strconv"
	"strings"
	"time"

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

type seqIntFlag []int

func (f *seqIntFlag) String() string {
	return fmt.Sprint(*f)
}

func (f *seqIntFlag) Set(value string) error {
	for _, v := range strings.Split(value, ",") {
		val, err := strconv.Atoi(v)
		if err != nil {
			return err
		}
		*f = append(*f, val)
	}
	return nil
}

var logs seqStringFlag
var logFds seqIntFlag

var (
	port    = flag.String("port", "3903", "HTTP port to listen on.")
	address = flag.String("address", "", "Host or IP address on which to bind HTTP listener")
	progs   = flag.String("progs", "", "Name of the directory containing mtail programs")

	// Compiler behaviour flags
	oneShot        = flag.Bool("one_shot", false, "Compile the programs, then read the contents of the provided logs from start until EOF, print the values of the metrics store and exit. This is a debugging flag only, not for production use.")
	oneShotMetrics = flag.Bool("one_shot_metrics", false, "DEPRECATED: Dump metrics (to stdout) after one shot mode.")
	compileOnly    = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")
	dumpAst        = flag.Bool("dump_ast", false, "Dump AST of programs after parse (to INFO log).")
	dumpAstTypes   = flag.Bool("dump_ast_types", false, "Dump AST of programs with type annotation after typecheck (to INFO log).")
	dumpBytecode   = flag.Bool("dump_bytecode", false, "Dump bytecode of programs (to INFO log).")

	// Runtime behaviour flags
	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
	overrideTimezone     = flag.String("override_timezone", "Local", "If set, use the provided timezone in timestamp conversion, instead of the local zone.")
	emitProgLabel        = flag.Bool("emit_prog_label", true, "Emit the 'prog' label in variable exports.")

	// Debugging flags
	blockProfileRate     = flag.Int("block_profile_rate", 0, "Nanoseconds of block time before goroutine blocking events reported. 0 turns off.  See https://golang.org/pkg/runtime/#SetBlockProfileRate")
	mutexProfileFraction = flag.Int("mutex_profile_fraction", 0, "Fraction of mutex contention events reported.  0 turns off.  See http://golang.org/pkg/runtime/#SetMutexProfileFraction")
)

func init() {
	flag.Var(&logs, "logs", "List of log files to monitor, separated by commas.  This flag may be specified multiple times.")
	flag.Var(&logFds, "logfds", "List of file descriptor numbers to monitor, separated by commas.  This flag may be specified multiple times.")
}

var (
	// Version and Revision are supplied by the linker
	Version  string
	Revision string

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
	glog.Infof("Commandline: %q", os.Args)
	loc, err := time.LoadLocation(*overrideTimezone)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Couldn't parse timezone %q: %s", *overrideTimezone, err)
		os.Exit(1)
	}
	if *blockProfileRate > 0 {
		glog.Infof("Setting block profile rate to %d", *blockProfileRate)
		runtime.SetBlockProfileRate(*blockProfileRate)
	}
	if *mutexProfileFraction > 0 {
		glog.Infof("Setting mutex profile fraction to %d", *mutexProfileFraction)
		SetMutexProfileFraction(*mutexProfileFraction)
	}
	if *progs == "" {
		glog.Exitf("No mtail program directory specified; use -progs")
	}
	if !(*dumpBytecode || *dumpAst || *dumpAstTypes || *compileOnly) {
		if len(logs) == 0 && len(logFds) == 0 {
			glog.Exitf("No logs specified to tail; use -logs or -logfds")
		}
	}
	o := mtail.Options{
		Progs:                *progs,
		LogPathPatterns:      logs,
		LogFds:               logFds,
		BindAddress:          net.JoinHostPort(*address, *port),
		OneShot:              *oneShot,
		CompileOnly:          *compileOnly,
		DumpAst:              *dumpAst,
		DumpAstTypes:         *dumpAstTypes,
		DumpBytecode:         *dumpBytecode,
		SyslogUseCurrentYear: *syslogUseCurrentYear,
		OverrideLocation:     loc,
		OmitProgLabel:        !*emitProgLabel,
		BuildInfo:            buildInfo(),
	}
	m, err := mtail.New(o)
	if err != nil {
		glog.Fatalf("couldn't start: %s", err)
	}
	err = m.Run()
	if err != nil {
		glog.Exit(err)
	}
}
