// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"github.com/google/mtail/internal/logline"
)

func LinesReceived(lines <-chan *logline.LogLine) (r []*logline.LogLine) {
	r = make([]*logline.LogLine, 0)
	for line := range lines {
		r = append(r, line)
	}
	return
}
