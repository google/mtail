// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/bytecode"
)

// Object is the data and bytecode resulting from compiled program source.
type Object struct {
	Program []bytecode.Instr  // The program bytecode.
	Strings []string          // Static strings.
	Regexps []*regexp.Regexp  // Static regular expressions.
	Metrics []*metrics.Metric // Metrics accessible to this program.
}
