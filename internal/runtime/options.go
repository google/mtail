// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

import (
	"time"

	"github.com/google/mtail/internal/runtime/vm"
	"github.com/prometheus/client_golang/prometheus"
)

// Option configures a new program Runtime.
type Option func(*Runtime) error

// OverrideLocation sets the timezone location for the VM.
func OverrideLocation(loc *time.Location) Option {
	return func(l *Runtime) error {
		l.overrideLocation = loc
		return nil
	}
}

// CompileOnly sets the Runtime to compile programs only, without executing them.
func CompileOnly() Option {
	return func(l *Runtime) error {
		l.compileOnly = true
		return ErrorsAbort()(l)
	}
}

// ErrorsAbort sets the Runtime to abort the Runtime on compile errors.
func ErrorsAbort() Option {
	return func(l *Runtime) error {
		l.errorsAbort = true
		return nil
	}
}

// DumpAst instructs the Runtime to print the AST after program compilation.
func DumpAst() Option {
	return func(l *Runtime) error {
		l.dumpAst = true
		return nil
	}
}

// DumpAstTypes instructs the Runtime to print the AST after type checking.
func DumpAstTypes() Option {
	return func(l *Runtime) error {
		l.dumpAstTypes = true
		return nil
	}
}

// DumpBytecode instructs the loader to print the compiled bytecode after code generation.
func DumpBytecode() Option {
	return func(l *Runtime) error {
		l.dumpBytecode = true
		return nil
	}
}

// SyslogUseCurrentYear instructs the VM to annotate yearless timestamps with the current year.
func SyslogUseCurrentYear() Option {
	return func(l *Runtime) error {
		l.syslogUseCurrentYear = true
		return nil
	}
}

// MaxRegexpLength sets the maximum length an mtail regular expression can have, in terms of characters.
func MaxRegexpLength(maxRegexpLength int) Option {
	return func(l *Runtime) error {
		l.maxRegexpLength = maxRegexpLength
		return nil
	}
}

// MaxRecursionDepth sets the maximum depth the abstract syntax tree built during lexation can have
func MaxRecursionDepth(maxRecursionLength int) Option {
	return func(l *Runtime) error {
		l.maxRecursionDepth = maxRecursionLength
		return nil
	}
}

// OmitMetricSource instructs the Runtime to not annotate metrics with their program source when added to the metric store.
func OmitMetricSource() Option {
	return func(l *Runtime) error {
		l.omitMetricSource = true
		return nil
	}
}

// PrometheusRegisterer passes in a registry for setting up exported metrics.
func PrometheusRegisterer(reg prometheus.Registerer) Option {
	return func(l *Runtime) error {
		l.reg = reg
		l.reg.MustRegister(vm.LineProcessingDurations)
		return nil
	}
}

// LogRuntimeErrors instructs the VM to emit runtime errors into the log.
func LogRuntimeErrors() Option {
	return func(l *Runtime) error {
		l.logRuntimeErrors = true
		return nil
	}
}
