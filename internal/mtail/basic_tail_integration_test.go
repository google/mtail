// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"fmt"
	"path"
	"sync"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestBasicTail(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "tail=2,log_watcher=2")()
	}
	for _, test := range mtail.LogWatcherTestTable {
		t.Run(fmt.Sprintf("%s %v", test.PollInterval, test.EnableFsNotify), func(t *testing.T) {
			logDir, rmLogDir := testutil.TestTempDir(t)
			defer rmLogDir()

			m, stopM := mtail.TestStartServer(t, test.PollInterval, test.EnableFsNotify, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
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
