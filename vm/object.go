// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp"

	"github.com/google/mtail/metrics"
)

// object describes a built object of data and bytecode
type object struct {
	prog []instr           // The emitted program.
	l    []int             // Jump labels
	str  []string          // Static strings.
	re   []*regexp.Regexp  // Static regular expressions.
	m    []*metrics.Metric // Metrics accessible to this program.
}
