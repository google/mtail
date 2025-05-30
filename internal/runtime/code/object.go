// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package code

import (
	"regexp"

	"github.com/jaqx0r/mtail/internal/metrics"
)

// Object is the data and bytecode resulting from compiled program source.
type Object struct {
	Program []Instr           // The program bytecode.
	Strings []string          // Static strings.
	Regexps []*regexp.Regexp  // Static regular expressions.
	Metrics []*metrics.Metric // Metrics accessible to this program.
}
