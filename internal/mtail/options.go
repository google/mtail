// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"net"
	"time"
)

// ProgramPath sets the path to find mtail programs in the MtailServer.
func ProgramPath(path string) func(*Server) error {
	return func(m *Server) error {
		m.programPath = path
		return nil
	}
}

// LogPathPatterns sets the patterns to find log paths in the MtailServer.
func LogPathPatterns(patterns ...string) func(*Server) error {
	return func(m *Server) error {
		m.logPathPatterns = patterns
		return nil
	}
}

// BindAddress sets the HTTP server address in MtailServer.
func BindAddress(address, port string) func(*Server) error {
	return func(m *Server) error {
		m.bindAddress = net.JoinHostPort(address, port)
		var err error
		m.listener, err = net.Listen("tcp", m.bindAddress)
		return err
	}
}

// BuildInfo sets the mtail program build information in the MtailServer.
func BuildInfo(info string) func(*Server) error {
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

// StoreExpireTickInterval sets the interval to run ticker to delete expired metrics from store.
func StoreExpireTickInterval(interval *time.Duration) func(*Server) error {
	return func(m *Server) error {
		m.storeExpireTickInterval = interval
		return nil
	}
}

// OneShot sets one-shot mode in the MtailServer.
func OneShot(m *Server) error {
	m.oneShot = true
	return nil
}

// CompileOnly sets compile-only mode in the MtailServer.
func CompileOnly(m *Server) error {
	m.compileOnly = true
	return nil
}

// DumpAst instructs the MtailServer's compiler to print the AST after parsing.
func DumpAst(m *Server) error {
	m.dumpAst = true
	return nil
}

// DumpAstTypes instructs the MtailServer's copmiler to print the AST after type checking.
func DumpAstTypes(m *Server) error {
	m.dumpAstTypes = true
	return nil
}

// DumpBytecode instructs the MtailServer's compiuler to print the program bytecode after code generation.
func DumpBytecode(m *Server) error {
	m.dumpBytecode = true
	return nil
}

// SyslogUseCurrentYear instructs the MtailServer to use the current year for year-less log timestamp during parsing.
func SyslogUseCurrentYear(m *Server) error {
	m.syslogUseCurrentYear = true
	return nil
}

// OmitProgLabel sets the MtailServer to not put the program name as a label in exported metrics.
func OmitProgLabel(m *Server) error {
	m.omitProgLabel = true
	return nil
}

// OmitMetricSource sets the MtailServer to not link created metrics to their source program.
func OmitMetricSource(m *Server) error {
	m.omitMetricSource = true
	return nil
}
