// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"fmt"
	"path"
	"sync"
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
		{0, true},                      // notify only
		{10 * time.Millisecond, false}, // poll only
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

			lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 3)
			logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)

			logFile := path.Join(logDir, "log")

			f := testutil.TestOpenFile(t, logFile)

			for i := 1; i <= 3; i++ {
				testutil.WriteString(t, f, fmt.Sprintf("%d\n", i))
			}

			var wg sync.WaitGroup
			wg.Add(2)
			go func() {
				defer wg.Done()
				lineCountCheck()
			}()
			go func() {
				defer wg.Done()
				logCountCheck()
			}()
			wg.Wait()
		})
	}
}
