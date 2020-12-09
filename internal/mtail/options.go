// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"net"
	"time"

	"contrib.go.opencensus.io/exporter/jaeger"
	"go.opencensus.io/trace"
)

// ProgramPath sets the path to find mtail programs in the Server.
func ProgramPath(path string) func(*Server) error {
	return func(m *Server) error {
		m.programPath = path
		return nil
	}
}

// LogPathPatterns sets the patterns to find log paths in the Server.
func LogPathPatterns(patterns ...string) func(*Server) error {
	return func(m *Server) error {
		m.logPathPatterns = patterns
		return nil
	}
}

// IgnoreRegexPattern sets the regex pattern to ignore files.
func IgnoreRegexPattern(pattern string) func(*Server) error {
	return func(m *Server) error {
		m.ignoreRegexPattern = pattern
		return nil
	}
}

// BindAddress sets the HTTP server address in Server.
func BindAddress(address, port string) func(*Server) error {
	return func(m *Server) error {
		m.bindAddress = net.JoinHostPort(address, port)
		var err error
		m.listener, err = net.Listen("tcp", m.bindAddress)
		return err
	}
}

// BindUnixSocket sets the UNIX socket path in Server.
func BindUnixSocket(unixSocketPath string) func(*Server) error {
	return func(m *Server) error {
		m.bindUnixSocket = unixSocketPath
		var err error
		m.listener, err = net.Listen("unix", unixSocketPath)
		return err
	}
}

// SetBuildInfo sets the mtail program build information in the Server.
func SetBuildInfo(info BuildInfo) func(*Server) error {
	return func(m *Server) error {
		m.buildInfo = info
		return nil
	}
}

// OverrideLocation sets the timezone location for log timestamps without any such information.
func OverrideLocation(loc *time.Location) func(*Server) error {
	return func(m *Server) error {
		m.overrideLocation = loc
		return nil
	}
}

// ExpiredMetricGcTickInterval sets the interval to run ticker to delete expired metrics from store.
func ExpiredMetricGcTickInterval(interval time.Duration) func(*Server) error {
	return func(m *Server) error {
		m.expiredMetricGcTickInterval = interval
		return nil
	}
}

// StaleLogGcTickInterval sets the interval to run ticker to remove stale log handles.
func StaleLogGcTickInterval(interval time.Duration) func(*Server) error {
	return func(m *Server) error {
		m.staleLogGcTickInterval = interval
		return nil
	}
}

// OneShot sets one-shot mode in the Server.
func OneShot(m *Server) error {
	m.oneShot = true
	return nil
}

// CompileOnly sets compile-only mode in the Server.
func CompileOnly(m *Server) error {
	m.compileOnly = true
	return nil
}

// DumpAst instructs the Server's compiler to print the AST after parsing.
func DumpAst(m *Server) error {
	m.dumpAst = true
	return nil
}

// DumpAstTypes instructs the Server's copmiler to print the AST after type checking.
func DumpAstTypes(m *Server) error {
	m.dumpAstTypes = true
	return nil
}

// DumpBytecode instructs the Server's compiuler to print the program bytecode after code generation.
func DumpBytecode(m *Server) error {
	m.dumpBytecode = true
	return nil
}

// SyslogUseCurrentYear instructs the Server to use the current year for year-less log timestamp during parsing.
func SyslogUseCurrentYear(m *Server) error {
	m.syslogUseCurrentYear = true
	return nil
}

// OmitProgLabel sets the Server to not put the program name as a label in exported metrics.
func OmitProgLabel(m *Server) error {
	m.omitProgLabel = true
	return nil
}

// OmitMetricSource sets the Server to not link created metrics to their source program.
func OmitMetricSource(m *Server) error {
	m.omitMetricSource = true
	return nil
}

// EmitMetricTimestamp tells the Server to export the metric's timestamp.
func EmitMetricTimestamp(m *Server) error {
	m.emitMetricTimestamp = true
	return nil
}

// JaegerReporter creates a new jaeger reporter that sends to the given Jaeger endpoint address.
func JaegerReporter(jaegerEndpoint string) func(*Server) error {
	return func(m *Server) error {
		je, err := jaeger.NewExporter(jaeger.Options{
			CollectorEndpoint: jaegerEndpoint,
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
}

//
func OmitDumpMetricStore(m *Server) error {
	m.omitDumpMetricsStore = true
	return nil
}
