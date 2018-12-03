// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp"

	"github.com/google/mtail/internal/metrics"
)

// object describes a built object of data and bytecode
type object struct {
	prog []Instr           // The emitted program.
	str  []string          // Static strings.
	re   []*regexp.Regexp  // Static regular expressions.
	m    []*metrics.Metric // Metrics accessible to this program.
}
