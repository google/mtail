// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
)

func LinesReceived(lines <-chan *logline.LogLine) (r []*logline.LogLine) {
	r = make([]*logline.LogLine, 0)
	for line := range lines {
		r = append(r, line)
	}
	return
}

func ExpectLinesReceivedNoDiff(tb testing.TB, wantLines []*logline.LogLine, gotLines <-chan *logline.LogLine) func() {
	tb.Helper()
	var received []*logline.LogLine

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		for line := range gotLines {
			received = append(received, line)
		}
	}()
	return func() {
		tb.Helper()
		wg.Wait()
		ExpectNoDiff(tb, wantLines, received, IgnoreFields(logline.LogLine{}, "Context"))
	}
}
