// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"os/signal"
	"runtime"
	"strings"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/exporter"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/waker"
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

	// Compiler behaviour flags.
	oneShot       = flag.Bool("one_shot", false, "Compile the programs, then read the contents of the provided logs from start until EOF, print the values of the metrics store in the given format and exit. This is a debugging flag only, not for production use.")
	oneShotFormat = flag.String("one_shot_format", "json", "Format to use with -one_shot. This is a debugging flag only, not for production use. Supported formats: json, prometheus.")
	compileOnly   = flag.Bool("compile_only", false, "Compile programs only, do not load the virtual machine.")
	dumpAst       = flag.Bool("dump_ast", false, "Dump AST of programs after parse (to INFO log).")
	dumpAstTypes  = flag.Bool("dump_ast_types", false, "Dump AST of programs with type annotation after typecheck (to INFO log).")
	dumpBytecode  = flag.Bool("dump_bytecode", false, "Dump bytecode of programs (to INFO log).")

	// VM Runtime behaviour flags.
	syslogUseCurrentYear = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
	overrideTimezone     = flag.String("override_timezone", "", "If set, use the provided timezone in timestamp conversion, instead of UTC.")
	emitProgLabel        = flag.Bool("emit_prog_label", true, "Emit the 'prog' label in variable exports.")
	emitMetricTimestamp  = flag.Bool("emit_metric_timestamp", false, "Emit the recorded timestamp of a metric.  If disabled (the default) no explicit timestamp is sent to a collector.")
	emitGaugeArray       = flag.Bool("emit_gauge_array", false, "Emit an array of values when one gauge has multiple metric values.  If disabled (the default), one value is emitted per gauge.")
	logRuntimeErrors     = flag.Bool("vm_logs_runtime_errors", true, "Enables logging of runtime errors to the standard log.  Set to false to only have the errors printed to the HTTP console.")

	// Ops flags.
	pollInterval                = flag.Duration("poll_interval", 250*time.Millisecond, "Set the interval to poll each log file for data; must be positive, or zero to disable polling.  With polling mode, only the files found at mtail startup will be polled.")
	pollLogInterval             = flag.Duration("poll_log_interval", 250*time.Millisecond, "Set the interval to find all matched log files for polling; must be positive, or zero to disable polling.  With polling mode, only the files found at mtail startup will be polled.")
	expiredMetricGcTickInterval = flag.Duration("expired_metrics_gc_interval", time.Hour, "interval between expired metric garbage collection runs")
	staleLogGcTickInterval      = flag.Duration("stale_log_gc_interval", time.Hour, "interval between stale log garbage collection runs")
	metricPushInterval          = flag.Duration("metric_push_interval", time.Minute, "interval between metric pushes to passive collectors")
	maxRegexpLength             = flag.Int("max_regexp_length", 1024, "The maximum length a mtail regexp expression can have. Excessively long patterns are likely to cause compilation and runtime performance problems.")
	maxRecursionDepth           = flag.Int("max_recursion_depth", 100, "The maximum length a mtail statement can be, as measured by parsed tokens. Excessively long mtail expressions are likely to cause compilation and runtime performance problems.")

	// Debugging flags.
	blockProfileRate     = flag.Int("block_profile_rate", 0, "Nanoseconds of block time before goroutine blocking events reported. 0 turns off.  See https://golang.org/pkg/runtime/#SetBlockProfileRate")
	mutexProfileFraction = flag.Int("mutex_profile_fraction", 0, "Fraction of mutex contention events reported.  0 turns off.  See http://golang.org/pkg/runtime/#SetMutexProfileFraction")
	httpDebugEndpoints   = flag.Bool("http_debugging_endpoint", true, "Enable debugging endpoints (/debug/*).")
	httpInfoEndpoints    = flag.Bool("http_info_endpoint", true, "Enable info endpoints (/progz,/varz).")

	// Tracing.
	jaegerEndpoint    = flag.String("jaeger_endpoint", "", "If set, collector endpoint URL of jaeger thrift service")
	traceSamplePeriod = flag.Int("trace_sample_period", 0, "Sample period for traces.  If non-zero, every nth trace will be sampled.")

	// Deprecated.
	_ = flag.Bool("disable_fsnotify", true, "DEPRECATED: this flag is no longer in use.")
	_ = flag.Int("metric_push_interval_seconds", 0, "DEPRECATED: use --metric_push_interval instead")
)

func init() {
	flag.Var(&logs, "logs", "List of log files to monitor, separated by commas.  This flag may be specified multiple times.")
}

var (
	// Branch as well as Version and Revision identifies where in the git
	// history the build came from, as supplied by the linker when copmiled
	// with `make'.  The defaults here indicate that the user did not use
	// `make' as instructed.
	Branch   = "invalid:-use-make-to-build"
	Version  = "invalid:-use-make-to-build"
	Revision = "invalid:-use-make-to-build"
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
		glog.Exitf("mtail requires programs that instruct it how to extract metrics from logs; please use the flag -progs to specify the directory containing the programs.")
	}
	if !(*dumpBytecode || *dumpAst || *dumpAstTypes || *compileOnly) {
		if len(logs) == 0 {
			glog.Exitf("mtail requires the names of logs to follow in order to extract logs from them; please use the flag -logs one or more times to specify glob patterns describing these logs.")
		}
	}

	if *traceSamplePeriod > 0 {
		trace.ApplyConfig(trace.Config{DefaultSampler: trace.ProbabilitySampler(1 / float64(*traceSamplePeriod))})
	}
	if *pollInterval == 0 {
		glog.Infof("no poll log data interval specified; defaulting to 250ms poll")
		*pollInterval = time.Millisecond * 250
	}
	if *pollLogInterval == 0 {
		glog.Infof("no poll log pattern interval specified; defaulting to 250ms poll")
		*pollLogInterval = time.Millisecond * 250
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	sigint := make(chan os.Signal, 1)
	signal.Notify(sigint, os.Interrupt, syscall.SIGTERM)
	go func() {
		sig := <-sigint
		glog.Infof("Received %+v, exiting...", sig)
		cancel()
	}()

	opts := []mtail.Option{
		mtail.ProgramPath(*progs),
		mtail.LogPathPatterns(logs...),
		mtail.IgnoreRegexPattern(*ignoreRegexPattern),
		mtail.SetBuildInfo(buildInfo),
		mtail.OverrideLocation(loc),
		mtail.MetricPushInterval(*metricPushInterval),
		mtail.MaxRegexpLength(*maxRegexpLength),
		mtail.MaxRecursionDepth(*maxRecursionDepth),
	}
	eOpts := []exporter.Option{}
	if *logRuntimeErrors {
		opts = append(opts, mtail.LogRuntimeErrors)
	}
	if *staleLogGcTickInterval > 0 {
		staleLogGcWaker := waker.NewTimed(ctx, *staleLogGcTickInterval)
		opts = append(opts, mtail.StaleLogGcWaker(staleLogGcWaker))
	}
	if *pollInterval > 0 {
		logStreamPollWaker := waker.NewTimed(ctx, *pollInterval)
		logPatternPollWaker := waker.NewTimed(ctx, *pollLogInterval)
		opts = append(opts, mtail.LogPatternPollWaker(logPatternPollWaker), mtail.LogstreamPollWaker(logStreamPollWaker))
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
	if *httpDebugEndpoints {
		opts = append(opts, mtail.HTTPDebugEndpoints)
	}
	if *httpInfoEndpoints {
		opts = append(opts, mtail.HTTPInfoEndpoints)
	}
	if *syslogUseCurrentYear {
		opts = append(opts, mtail.SyslogUseCurrentYear)
	}
	if !*emitProgLabel {
		opts = append(opts, mtail.OmitProgLabel)
		eOpts = append(eOpts, exporter.OmitProgLabel())
	}
	if *emitMetricTimestamp {
		opts = append(opts, mtail.EmitMetricTimestamp)
		eOpts = append(eOpts, exporter.EmitTimestamp())
	}
	if *emitGaugeArray {
		opts = append(opts, mtail.EmitGaugeArray)
		eOpts = append(eOpts, exporter.EmitGaugeArray())
	}
	if *jaegerEndpoint != "" {
		opts = append(opts, mtail.JaegerReporter(*jaegerEndpoint))
	}
	store := metrics.NewStore()
	if *expiredMetricGcTickInterval > 0 {
		store.StartGcLoop(ctx, *expiredMetricGcTickInterval)
	}
	m, err := mtail.New(ctx, store, opts...)
	if err != nil {
		glog.Error(err)
		cancel()
		os.Exit(1) //nolint:gocritic // false positive
	}
	err = m.Run()
	if err != nil {
		glog.Error(err)
		cancel()
		os.Exit(1) //nolint:gocritic // false positive
	}
	if *oneShot {
		switch *oneShotFormat {
		case "prometheus":
			e, err := exporter.New(ctx, nil, store, eOpts...)
			if err != nil {
				glog.Error(err)
				cancel()
				os.Exit(1) //nolint:gocritic // false positive
			}
			err = e.Write(os.Stdout)
			if err != nil {
				glog.Error(err)
				cancel()
				os.Exit(1) //nolint:gocritic // false positive
			}
			cancel()
			os.Exit(0) //nolint:gocritic // false positive
		case "json":
			err = store.WriteMetrics(os.Stdout)
			if err != nil {
				glog.Error(err)
				os.Exit(1) //nolint:gocritic // false positive
			}
			cancel()
			os.Exit(0) //nolint:gocritic // false positive
		default:
			glog.Errorf("unsupported format: %q", *oneShotFormat)
			cancel()
			os.Exit(1) //nolint:gocritic // false positive
		}
	}
}
