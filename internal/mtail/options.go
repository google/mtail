// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"fmt"
	"net"
	"time"

	"contrib.go.opencensus.io/exporter/jaeger"
	"github.com/google/mtail/internal/waker"
	"go.opencensus.io/trace"
)

// Option configures mtail.Server
type Option interface {
	apply(*Server) error
}

// ProgramPath sets the path to find mtail programs in the Server.
type ProgramPath string

func (opt ProgramPath) apply(m *Server) error {
	m.programPath = string(opt)
	return nil
}

// LogPathPatterns sets the patterns to find log paths in the Server.
func LogPathPatterns(patterns ...string) Option {
	return logPathPatterns(patterns)
}

type logPathPatterns []string

func (opt logPathPatterns) apply(m *Server) error {
	m.logPathPatterns = opt
	return nil
}

// IgnoreRegexPattern sets the regex pattern to ignore files.
type IgnoreRegexPattern string

func (opt IgnoreRegexPattern) apply(m *Server) error {
	m.ignoreRegexPattern = string(opt)
	return nil
}

// BindAddress sets the HTTP server address in Server.
func BindAddress(address, port string) Option {
	return &bindAddress{address, port}
}

type bindAddress struct {
	address, port string
}

func (opt bindAddress) apply(m *Server) error {
	if m.listener != nil {
		return fmt.Errorf("HTTP server bind address already supplied")
	}
	m.bindAddress = net.JoinHostPort(opt.address, opt.port)
	var err error
	m.listener, err = net.Listen("tcp", m.bindAddress)
	return err
}

// BindUnixSocket sets the UNIX socket path in Server.
type BindUnixSocket string

func (opt BindUnixSocket) apply(m *Server) error {
	if m.listener != nil {
		return fmt.Errorf("HTTP server bind address already supplied")
	}
	m.bindUnixSocket = string(opt)
	var err error
	m.listener, err = net.Listen("unix", string(opt))
	return err
}

// SetBuildInfo sets the mtail program build information in the Server.
type SetBuildInfo BuildInfo

func (opt SetBuildInfo) apply(m *Server) error {
	m.buildInfo = BuildInfo(opt)
	return nil
}

// OverrideLocation sets the timezone location for log timestamps without any such information.
func OverrideLocation(loc *time.Location) Option {
	return &overrideLocation{loc}
}

type overrideLocation struct {
	*time.Location
}

func (opt overrideLocation) apply(m *Server) error {
	m.overrideLocation = opt.Location
	return nil
}

// StaleLogGcTickInterval sets the interval between garbage collection runs for
// stale logs in the tailer.
type StaleLogGcTickInterval time.Duration

func (opt StaleLogGcTickInterval) apply(m *Server) error {
	m.staleLogGcWaker, m.stopStaleLogGcWaker = waker.NewTimed(time.Duration(opt))
	return nil
}

// LogPatternPollTickInterval sets the interval between polls on the filesystem for new logs that match the log glob patterns.
type LogPatternPollTickInterval time.Duration

func (opt LogPatternPollTickInterval) apply(m *Server) error {
	m.logPatternPollWaker, m.stopLogPatternPollWaker = waker.NewTimed(time.Duration(opt))
	return nil
}

type niladicOption struct {
	applyfunc func(m *Server) error
}

func (n *niladicOption) apply(m *Server) error {
	return n.applyfunc(m)
}

// OneShot sets one-shot mode in the Server.
var OneShot = &niladicOption{
	func(m *Server) error {
		m.oneShot = true
		return nil
	}}

// CompileOnly sets compile-only mode in the Server.
var CompileOnly = &niladicOption{
	func(m *Server) error {
		m.compileOnly = true
		return nil
	}}

// DumpAst instructs the Server's compiler to print the AST after parsing.
var DumpAst = &niladicOption{
	func(m *Server) error {
		m.dumpAst = true
		return nil
	}}

// DumpAstTypes instructs the Server's copmiler to print the AST after type checking.
var DumpAstTypes = &niladicOption{
	func(m *Server) error {
		m.dumpAstTypes = true
		return nil
	}}

// DumpBytecode instructs the Server's compiuler to print the program bytecode after code generation.
var DumpBytecode = &niladicOption{
	func(m *Server) error {
		m.dumpBytecode = true
		return nil
	}}

// SyslogUseCurrentYear instructs the Server to use the current year for year-less log timestamp during parsing.
var SyslogUseCurrentYear = &niladicOption{
	func(m *Server) error {
		m.syslogUseCurrentYear = true
		return nil
	}}

// OmitProgLabel sets the Server to not put the program name as a label in exported metrics.
var OmitProgLabel = &niladicOption{
	func(m *Server) error {
		m.omitProgLabel = true
		return nil
	}}

// OmitMetricSource sets the Server to not link created metrics to their source program.
var OmitMetricSource = &niladicOption{
	func(m *Server) error {
		m.omitMetricSource = true
		return nil
	}}

// EmitMetricTimestamp tells the Server to export the metric's timestamp.
var EmitMetricTimestamp = &niladicOption{
	func(m *Server) error {
		m.emitMetricTimestamp = true
		return nil
	}}

// JaegerReporter creates a new jaeger reporter that sends to the given Jaeger endpoint address.
type JaegerReporter string

func (opt JaegerReporter) apply(m *Server) error {
	je, err := jaeger.NewExporter(jaeger.Options{
		CollectorEndpoint: string(opt),
		Process: jaeger.Process{
			ServiceName: "mtail",
		},
	})
	if err != nil {
		return err
	}
	trace.RegisterExporter(je)
	return nil
}

// OmitDumpMetricStore disables dumping of the metric store... somewhere.
var OmitDumpMetricStore = &niladicOption{
	func(m *Server) error {
		m.omitDumpMetricsStore = true
		return nil
	}}
