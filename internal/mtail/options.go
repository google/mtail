// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"errors"
	"net"
	"os"
	"path/filepath"
	"time"

	"contrib.go.opencensus.io/exporter/jaeger"
	"github.com/google/mtail/internal/exporter"
	"github.com/google/mtail/internal/runtime"
	"github.com/google/mtail/internal/tailer"
	"github.com/google/mtail/internal/waker"
	"go.opencensus.io/trace"
)

// Option configures mtail.Server.
type Option interface {
	apply(*Server) error
}

// ProgramPath sets the path to find mtail programs in the Server.
type ProgramPath string

func (opt ProgramPath) apply(m *Server) error {
	m.programPath = filepath.Clean(string(opt))
	if _, err := os.Stat(m.programPath); os.IsNotExist(err) {
		return err
	}
	return nil
}

// LogPathPatterns sets the patterns to find log paths in the Server.
func LogPathPatterns(patterns ...string) Option {
	return logPathPatterns(patterns)
}

type logPathPatterns []string

func (opt logPathPatterns) apply(m *Server) error {
	m.tOpts = append(m.tOpts, tailer.LogPatterns(opt))
	return nil
}

// IgnoreRegexPattern sets the regex pattern to ignore files.
type IgnoreRegexPattern string

func (opt IgnoreRegexPattern) apply(m *Server) error {
	m.tOpts = append(m.tOpts, tailer.IgnoreRegex(string(opt)))
	return nil
}

// BindAddress sets the HTTP server address in Server.
func BindAddress(address, port string) Option {
	return &bindAddress{address, port}
}

type bindAddress struct {
	address, port string
}

var ErrDuplicateHTTPBindAddress = errors.New("HTTP server bind address already supplied")

func (opt bindAddress) apply(m *Server) error {
	if m.listener != nil {
		return ErrDuplicateHTTPBindAddress
	}
	bindAddress := net.JoinHostPort(opt.address, opt.port)
	var err error
	m.listener, err = net.Listen("tcp", bindAddress)
	return err
}

// BindUnixSocket sets the UNIX socket path in Server.
type BindUnixSocket string

func (opt BindUnixSocket) apply(m *Server) error {
	if m.listener != nil {
		return ErrDuplicateHTTPBindAddress
	}
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
	m.rOpts = append(m.rOpts, runtime.OverrideLocation(opt.Location))
	return nil
}

// StaleLogGcWaker triggers garbage collection runs for stale logs in the tailer.
func StaleLogGcWaker(w waker.Waker) Option {
	return &staleLogGcWaker{w}
}

type staleLogGcWaker struct {
	waker.Waker
}

func (opt staleLogGcWaker) apply(m *Server) error {
	m.tOpts = append(m.tOpts, tailer.StaleLogGcWaker(opt.Waker))
	return nil
}

// LogPatternPollWaker triggers polls on the filesystem for new logs that match the log glob patterns.
func LogPatternPollWaker(w waker.Waker) Option {
	return &logPatternPollWaker{w}
}

type logPatternPollWaker struct {
	waker.Waker
}

func (opt logPatternPollWaker) apply(m *Server) error {
	m.tOpts = append(m.tOpts, tailer.LogPatternPollWaker(opt.Waker))
	return nil
}

// LogstreamPollWaker triggers polls on the filesystem for new logs that match the log glob streams.
func LogstreamPollWaker(w waker.Waker) Option {
	return &logstreamPollWaker{w}
}

type logstreamPollWaker struct {
	waker.Waker
}

func (opt logstreamPollWaker) apply(m *Server) error {
	m.tOpts = append(m.tOpts, tailer.LogstreamPollWaker(opt.Waker))
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
		m.rOpts = append(m.rOpts, runtime.ErrorsAbort())
		m.tOpts = append(m.tOpts, tailer.OneShot)
		m.oneShot = true
		return nil
	},
}

// CompileOnly sets compile-only mode in the Server.
var CompileOnly = &niladicOption{
	func(m *Server) error {
		m.rOpts = append(m.rOpts, runtime.CompileOnly())
		m.compileOnly = true
		return nil
	},
}

// DumpAst instructs the Server's compiler to print the AST after parsing.
var DumpAst = &niladicOption{
	func(m *Server) error {
		m.rOpts = append(m.rOpts, runtime.DumpAst())
		return nil
	},
}

// DumpAstTypes instructs the Server's copmiler to print the AST after type checking.
var DumpAstTypes = &niladicOption{
	func(m *Server) error {
		m.rOpts = append(m.rOpts, runtime.DumpAstTypes())
		return nil
	},
}

// DumpBytecode instructs the Server's compiuler to print the program bytecode after code generation.
var DumpBytecode = &niladicOption{
	func(m *Server) error {
		m.rOpts = append(m.rOpts, runtime.DumpBytecode())
		return nil
	},
}

// HttpDebugEndpoints enables debug http endpoints
var HTTPDebugEndpoints = &niladicOption{
	func(m *Server) error {
		m.httpDebugEndpoints = true
		return nil
	},
}

// HttpInfoEndpoints enables info http endpoints
var HTTPInfoEndpoints = &niladicOption{
	func(m *Server) error {
		m.httpInfoEndpoints = true
		return nil
	},
}

// SyslogUseCurrentYear instructs the Server to use the current year for year-less log timestamp during parsing.
var SyslogUseCurrentYear = &niladicOption{
	func(m *Server) error {
		m.rOpts = append(m.rOpts, runtime.SyslogUseCurrentYear())
		return nil
	},
}

// OmitProgLabel sets the Server to not put the program name as a label in exported metrics.
var OmitProgLabel = &niladicOption{
	func(m *Server) error {
		m.eOpts = append(m.eOpts, exporter.OmitProgLabel())
		return nil
	},
}

// OmitMetricSource sets the Server to not link created metrics to their source program.
var OmitMetricSource = &niladicOption{
	func(m *Server) error {
		m.rOpts = append(m.rOpts, runtime.OmitMetricSource())
		return nil
	},
}

// EmitMetricTimestamp tells the Server to export the metric's timestamp.
var EmitMetricTimestamp = &niladicOption{
	func(m *Server) error {
		m.eOpts = append(m.eOpts, exporter.EmitTimestamp())
		return nil
	},
}

// EmitMetricTimestamp tells the Server to export a gauge array when applicable
var EmitGaugeArray = &niladicOption{
	func(m *Server) error {
		m.eOpts = append(m.eOpts, exporter.EmitGaugeArray())
		return nil
	},
}

// LogRuntimeErrors instructs the VM to emit runtime errors to the log.
var LogRuntimeErrors = &niladicOption{
	func(m *Server) error {
		m.rOpts = append(m.rOpts, runtime.LogRuntimeErrors())
		return nil
	},
}

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

// MetricPushInterval sets the interval between metrics pushes to passive collectors.
type MetricPushInterval time.Duration

func (opt MetricPushInterval) apply(m *Server) error {
	m.eOpts = append(m.eOpts, exporter.PushInterval(time.Duration(opt)))
	return nil
}

// MaxRegexpLength sets the maximum length an mtail regular expression can have, in terms of characters.
type MaxRegexpLength int

func (opt MaxRegexpLength) apply(m *Server) error {
	m.rOpts = append(m.rOpts, runtime.MaxRegexpLength(int(opt)))
	return nil
}

// MaxRecursionDepth sets the maximum depth the abstract syntax tree built during lexation can have.
type MaxRecursionDepth int

func (opt MaxRecursionDepth) apply(m *Server) error {
	m.rOpts = append(m.rOpts, runtime.MaxRecursionDepth(int(opt)))
	return nil
}
