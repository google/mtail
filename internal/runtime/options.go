// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

import (
	"time"

	"github.com/google/mtail/internal/runtime/compiler"
	"github.com/google/mtail/internal/runtime/vm"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
)

// Option configures a new program Runtime.
type Option func(*Runtime) error

// OverrideLocation sets the timezone location for the VM.
func OverrideLocation(loc *time.Location) Option {
	return func(r *Runtime) error {
		r.overrideLocation = loc
		return nil
	}
}

// CompileOnly sets the Runtime to compile programs only, without executing them.
func CompileOnly() Option {
	return func(r *Runtime) error {
		r.compileOnly = true
		return ErrorsAbort()(r)
	}
}

// ErrorsAbort sets the Runtime to abort the Runtime on compile errors.
func ErrorsAbort() Option {
	return func(r *Runtime) error {
		r.errorsAbort = true
		return nil
	}
}

// DumpAst emits the AST after program compilation.
func DumpAst() Option {
	return func(r *Runtime) error {
		r.cOpts = append(r.cOpts, compiler.EmitAst())
		return nil
	}
}

// DumpAstTypes emits the AST after type checking.
func DumpAstTypes() Option {
	return func(r *Runtime) error {
		r.cOpts = append(r.cOpts, compiler.EmitAstTypes())
		return nil
	}
}

// DumpBytecode instructs the loader to print the compiled bytecode after code generation.
func DumpBytecode() Option {
	return func(r *Runtime) error {
		r.dumpBytecode = true
		return nil
	}
}

// SyslogUseCurrentYear instructs the VM to annotate yearless timestamps with the current year.
func SyslogUseCurrentYear() Option {
	return func(r *Runtime) error {
		r.syslogUseCurrentYear = true
		return nil
	}
}

// MaxRegexpLength sets the maximum length an mtail regular expression can have, in terms of characters.
func MaxRegexpLength(maxRegexpLength int) Option {
	return func(r *Runtime) error {
		r.cOpts = append(r.cOpts, compiler.MaxRegexpLength(maxRegexpLength))
		return nil
	}
}

// MaxRecursionDepth sets the maximum depth the abstract syntax tree built during lexation can have.
func MaxRecursionDepth(maxRecursionDepth int) Option {
	return func(r *Runtime) error {
		r.cOpts = append(r.cOpts, compiler.MaxRecursionDepth(maxRecursionDepth))
		return nil
	}
}

// OmitMetricSource instructs the Runtime to not annotate metrics with their program source when added to the metric store.
func OmitMetricSource() Option {
	return func(r *Runtime) error {
		r.omitMetricSource = true
		return nil
	}
}

// PrometheusRegisterer passes in a registry for setting up exported metrics.
func PrometheusRegisterer(reg prometheus.Registerer) Option {
	return func(r *Runtime) error {
		r.reg = reg
		r.reg.MustRegister(vm.LineProcessingDurations)
		return nil
	}
}

// LogRuntimeErrors instructs the VM to emit runtime errors into the log.
func LogRuntimeErrors() Option {
	return func(r *Runtime) error {
		r.logRuntimeErrors = true
		return nil
	}
}

func TraceExecution() Option {
	return func(r *Runtime) error {
		r.trace = true
		return nil
	}
}

// UnmappedSourceBehavior sets how to handle log lines from sources that have no mapping.
// Valid values are "all" (default) or "none".
func UnmappedSourceBehavior(behavior string) Option {
	return func(r *Runtime) error {
		if behavior != "all" && behavior != "none" {
			return errors.Errorf("invalid unmapped behavior: %s (must be 'all' or 'none')", behavior)
		}
		r.unmappedBehavior = behavior
		return nil
	}
}
