// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"fmt"
	"path"
	"testing"
	"time"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestBasicTail(t *testing.T) {
	tests := []struct {
		pollInterval   time.Duration
		enableFsNotify bool
	}{
		{0, true},
		{10 * time.Millisecond, false},
	}
	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "tail=2,log_watcher=2")()
	}
	for _, test := range tests {
		t.Run(fmt.Sprintf("%s %v", test.pollInterval, test.enableFsNotify), func(t *testing.T) {
			logDir, rmLogDir := testutil.TestTempDir(t)
			defer rmLogDir()

			m, stopM := mtail.TestStartServer(t, test.pollInterval, test.enableFsNotify, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
			defer stopM()

			startLineCount := mtail.TestGetMetric(t, m.Addr(), "lines_total")

			time.Sleep(1 * time.Second)

			logFile := path.Join(logDir, "log")

			f := testutil.TestOpenFile(t, logFile)

			for i := 1; i <= 3; i++ {
				testutil.WriteString(t, f, fmt.Sprintf("%d\n", i))
				time.Sleep(1 * time.Second)
			}

			endLineCount := mtail.TestGetMetric(t, m.Addr(), "lines_total")

			lineCount := endLineCount.(float64) - startLineCount.(float64)
			if lineCount != 3. {
				t.Errorf("output didn't have expected line count increase: want 3 got %#v", lineCount)
				t.Logf("Line Count, and log lines total: %s, %s", mtail.TestGetMetric(t, m.Addr(), "lines_total"), mtail.TestGetMetric(t, m.Addr(), "log_lines_total"))
			}
		})
	}
}
