// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package object

import (
	"regexp"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/code"
)

// Object is the data and bytecode resulting from compiled program source.
type Object struct {
	Program []code.Instr      // The program bytecode.
	Strings []string          // Static strings.
	Regexps []*regexp.Regexp  // Static regular expressions.
	Metrics []*metrics.Metric // Metrics accessible to this program.
}
