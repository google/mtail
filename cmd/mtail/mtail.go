// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"fmt"
	"os"
	"runtime"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/mtail"
	"github.com/google/mtail/watcher"
	"github.com/spf13/afero"

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
	port    = flag.String("port", "3903", "HTTP port to listen on.")
	address = flag.String("address", "", "Host or IP address on which to bind HTTP listener")
	progs   = flag.String("progs", "", "Name of the directory containing mtail programs")

	version = flag.Bool("version", false, "Print mtail version information.")

	// Compiler behaviour flags
	oneShot        = flag.Bool("one_shot", false, "Compile the programs, then read the contents of the provided logs from start until EOF, print the values of the metrics store and exit. This is a debugging flag only, not for production use.")
	oneShotMetrics = flag.Bool("one_shot_metrics", false, "DEPRECATED: Dump metrics (to stdout) after one shot mode.")
	compileOnly    = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")
	dumpAst        = flag.Bool("dump_ast", false, "Dump AST of programs after parse (to INFO log).")
	dumpAstTypes   = flag.Bool("dump_ast_types", false, "Dump AST of programs with type annotation after typecheck (to INFO log).")
	dumpBytecode   = flag.Bool("dump_bytecode", false, "Dump bytecode of programs (to INFO log).")

	// VM Runtime behaviour flags
	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
	overrideTimezone     = flag.String("override_timezone", "", "If set, use the provided timezone in timestamp conversion, instead of UTC.")
	emitProgLabel        = flag.Bool("emit_prog_label", true, "Emit the 'prog' label in variable exports.")

	// Ops flags
	pollInterval = flag.Duration("poll_interval", 0, "Set the interval to poll all log files for data; must be positive, or zero to disable polling.")

	// Debugging flags
	blockProfileRate     = flag.Int("block_profile_rate", 0, "Nanoseconds of block time before goroutine blocking events reported. 0 turns off.  See https://golang.org/pkg/runtime/#SetBlockProfileRate")
	mutexProfileFraction = flag.Int("mutex_profile_fraction", 0, "Fraction of mutex contention events reported.  0 turns off.  See http://golang.org/pkg/runtime/#SetMutexProfileFraction")
)

func init() {
	flag.Var(&logs, "logs", "List of log files to monitor, separated by commas.  This flag may be specified multiple times.")
}

var (
	// Version and Revision are supplied by the linker
	Version  string
	Revision string

	GoVersion = runtime.Version()
)

func buildInfo() string {
	return fmt.Sprintf("mtail version %s git revision %s go version %s go arch %s go os %s", Version, Revision, GoVersion, runtime.GOARCH, runtime.GOOS)
}

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "%s\n", buildInfo())
		fmt.Fprintf(os.Stderr, "\nUsage:\n")
		flag.PrintDefaults()
	}
	flag.Parse()
	if *version {
		fmt.Println(buildInfo())
		os.Exit(1)
	}
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
		runtime.SetMutexProfileFraction(*mutexProfileFraction)
	}
	if *progs == "" {
		glog.Exitf("mtail requires programs that in instruct it how to extract metrics from logs; please use the flag -progs to specify the directory containing the programs.")
	}
	if !(*dumpBytecode || *dumpAst || *dumpAstTypes || *compileOnly) {
		if len(logs) == 0 {
			glog.Exitf("mtail requires the names of logs to follow in order to extract logs from them; please use the flag -logs one or more times to specify glob patterns describing these logs.")
		}
	}
	w, err := watcher.NewLogWatcher()
	if err != nil {
		glog.Exitf("Failure to create log watcher: %s", err)
	}
	opts := []func(*mtail.MtailServer) error{
		mtail.ProgramPath(*progs),
		mtail.LogPathPatterns(logs...),
		mtail.BindAddress(*address, *port),
		mtail.BuildInfo(buildInfo()),
		mtail.OverrideLocation(loc),
		mtail.PollInterval(*pollInterval),
	}
	if *oneShot {
		opts = append(opts, mtail.OneShot)
	}
	if *compileOnly {
		opts = append(opts, mtail.CompileOnly)
	}
	if *dumpAst {
		opts = append(opts, mtail.DumpAst)
	}
	if *dumpAstTypes {
		opts = append(opts, mtail.DumpAstTypes)
	}
	if *dumpBytecode {
		opts = append(opts, mtail.DumpBytecode)
	}
	if *syslogUseCurrentYear {
		opts = append(opts, mtail.SyslogUseCurrentYear)
	}
	if !*emitProgLabel {
		opts = append(opts, mtail.OmitProgLabel)
	}
	m, err := mtail.New(metrics.NewStore(), w, &afero.OsFs{}, opts...)
	if err != nil {
		glog.Error(err)
		os.Exit(1)
	}
	err = m.Run()
	if err != nil {
		glog.Error(err)
		os.Exit(1)
	}
}
