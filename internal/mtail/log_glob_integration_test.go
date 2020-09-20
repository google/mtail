// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"fmt"
	"os"
	"path"
	"sync"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestLogGlobMatchesAfterStartupWithPollInterval(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	for _, pollInterval := range []time.Duration{0, 10 * time.Millisecond} {
		t.Run(fmt.Sprintf("pollInterval=%s", pollInterval), func(t *testing.T) {
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()

			logDir := path.Join(tmpDir, "logs")
			progDir := path.Join(tmpDir, "progs")
			testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
			testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
			defer testutil.TestChdir(t, logDir)()

			m, stopM := mtail.TestStartServer(t, pollInterval, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log*"))
			defer stopM()

			{
				logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
				linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

				logFile := path.Join(logDir, "log")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
				testutil.FatalIfErr(t, err)
				glog.Infof("Wrote %d bytes", n)

				var wg sync.WaitGroup
				wg.Add(2)
				go func() {
					defer wg.Done()
					linesCountCheck()
				}()
				go func() {
					defer wg.Done()
					logCountCheck()
				}()
				wg.Wait()
			}
			{

				logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
				linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

				logFile := path.Join(logDir, "log1")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
				testutil.FatalIfErr(t, err)
				glog.Infof("Wrote %d bytes", n)
				var wg sync.WaitGroup
				wg.Add(2)
				go func() {
					defer wg.Done()
					linesCountCheck()
				}()
				go func() {
					defer wg.Done()
					logCountCheck()
				}()
				wg.Wait()
			}
		})
	}
}
