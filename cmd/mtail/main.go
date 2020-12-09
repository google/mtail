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
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/watcher"
	"go.opencensus.io/trace"
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
	port               = flag.String("port", "3903", "HTTP port to listen on.")
	address            = flag.String("address", "", "Host or IP address on which to bind HTTP listener")
	unixSocket         = flag.String("unix_socket", "", "UNIX Socket to listen on")
	progs              = flag.String("progs", "", "Name of the directory containing mtail programs")
	ignoreRegexPattern = flag.String("ignore_filename_regex_pattern", "", "")

	version = flag.Bool("version", false, "Print mtail version information.")

	// Compiler behaviour flags
	oneShot      = flag.Bool("one_shot", false, "Compile the programs, then read the contents of the provided logs from start until EOF, print the values of the metrics store and exit. This is a debugging flag only, not for production use.")
	compileOnly  = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")
	dumpAst      = flag.Bool("dump_ast", false, "Dump AST of programs after parse (to INFO log).")
	dumpAstTypes = flag.Bool("dump_ast_types", false, "Dump AST of programs with type annotation after typecheck (to INFO log).")
	dumpBytecode = flag.Bool("dump_bytecode", false, "Dump bytecode of programs (to INFO log).")

	// VM Runtime behaviour flags
	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
	overrideTimezone     = flag.String("override_timezone", "", "If set, use the provided timezone in timestamp conversion, instead of UTC.")
	emitProgLabel        = flag.Bool("emit_prog_label", true, "Emit the 'prog' label in variable exports.")
	emitMetricTimestamp  = flag.Bool("emit_metric_timestamp", false, "Emit the recorded timestamp of a metric.  If disabled (the default) no explicit timestamp is sent to a collector.")

	// Ops flags
	pollInterval                = flag.Duration("poll_interval", 0, "Set the interval to poll all log files for data; must be positive, or zero to disable polling.  With polling mode, only the files found at mtail startup will be polled.")
	disableFsnotify             = flag.Bool("disable_fsnotify", false, "EXPERIMENTAL: When enabled no fsnotify watcher is created, and mtail falls back to polling mode only.  Only the files known at program startup will be polled.")
	expiredMetricGcTickInterval = flag.Duration("expired_metrics_gc_interval", time.Hour, "interval between expired metric garbage collection runs")
	staleLogGcTickInterval      = flag.Duration("stale_log_gc_interval", time.Hour, "interval between stale log garbage collection runs")

	// Debugging flags
	blockProfileRate     = flag.Int("block_profile_rate", 0, "Nanoseconds of block time before goroutine blocking events reported. 0 turns off.  See https://golang.org/pkg/runtime/#SetBlockProfileRate")
	mutexProfileFraction = flag.Int("mutex_profile_fraction", 0, "Fraction of mutex contention events reported.  0 turns off.  See http://golang.org/pkg/runtime/#SetMutexProfileFraction")

	// Tracing
	jaegerEndpoint    = flag.String("jaeger_endpoint", "", "If set, collector endpoint URL of jaeger thrift service")
	traceSamplePeriod = flag.Int("trace_sample_period", 0, "Sample period for traces.  If non-zero, every nth trace will be sampled.")
)

func init() {
	flag.Var(&logs, "logs", "List of log files to monitor, separated by commas.  This flag may be specified multiple times.")
}

var (
	// Branch as well as Version and Revision identifies where in the git
	// history the build came from, as supplied by the linker when copmiled
	// with `make'.  The defaults here indicate that the user did not use
	// `make' as instructed.
	Branch   string = "invalid:-use-make-to-build"
	Version  string = "invalid:-use-make-to-build"
	Revision string = "invalid:-use-make-to-build"
)

func main() {
	buildInfo := mtail.BuildInfo{
		Branch:   Branch,
		Version:  Version,
		Revision: Revision,
	}

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "%s\n", buildInfo.String())
		fmt.Fprintf(os.Stderr, "\nUsage:\n")
		flag.PrintDefaults()
	}
	flag.Parse()
	if *version {
		fmt.Println(buildInfo.String())
		os.Exit(0)
	}
	glog.Info(buildInfo.String())
	glog.Infof("Commandline: %q", os.Args)
	if len(flag.Args()) > 0 {
		glog.Exitf("Too many extra arguments specified: %q\n(the logs flag can be repeated, or the filenames separated by commas.)", flag.Args())
	}
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

	if *traceSamplePeriod > 0 {
		trace.ApplyConfig(trace.Config{DefaultSampler: trace.ProbabilitySampler(1 / float64(*traceSamplePeriod))})
	}
	if *disableFsnotify && *pollInterval == 0 {
		glog.Infof("fsnotify disabled and no poll interval specified; defaulting to 250ms poll")
		*pollInterval = time.Millisecond * 250
	}
	w, err := watcher.NewLogWatcher(*pollInterval, !*disableFsnotify)
	if err != nil {
		glog.Exitf("Failure to create log watcher: %s", err)
	}
	opts := []func(*mtail.Server) error{
		mtail.ProgramPath(*progs),
		mtail.LogPathPatterns(logs...),
		mtail.IgnoreRegexPattern(*ignoreRegexPattern),
		mtail.SetBuildInfo(buildInfo),
		mtail.OverrideLocation(loc),
		mtail.ExpiredMetricGcTickInterval(*expiredMetricGcTickInterval),
		mtail.StaleLogGcTickInterval(*staleLogGcTickInterval),
	}
	if *unixSocket == "" {
		opts = append(opts, mtail.BindAddress(*address, *port))
	} else {
		opts = append(opts, mtail.BindUnixSocket(*unixSocket))
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
	if *emitMetricTimestamp {
		opts = append(opts, mtail.EmitMetricTimestamp)
	}
	if *jaegerEndpoint != "" {
		opts = append(opts, mtail.JaegerReporter(*jaegerEndpoint))
	}
	m, err := mtail.New(metrics.NewStore(), w, opts...)
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
