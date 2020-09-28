// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"fmt"
	"os"
	"path"
	"sync"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestGlobRelativeAfterStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	for _, test := range mtail.LogWatcherTestTable {
		t.Run(fmt.Sprintf("%s %v", test.PollInterval, test.EnableFsNotify), func(t *testing.T) {
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()

			logDir := path.Join(tmpDir, "logs")
			progDir := path.Join(tmpDir, "progs")
			err := os.Mkdir(logDir, 0700)
			testutil.FatalIfErr(t, err)
			err = os.Mkdir(progDir, 0700)
			testutil.FatalIfErr(t, err)
			defer testutil.TestChdir(t, logDir)()

			m, stopM := mtail.TestStartServer(t, test.PollInterval, test.EnableFsNotify, mtail.ProgramPath(progDir), mtail.LogPathPatterns("log.*"))
			defer stopM()

			{
				logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
				lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

				logFile := path.Join(logDir, "log.1.txt")
				f := testutil.TestOpenFile(t, logFile)

				n, err := f.WriteString("line 1\n")
				testutil.FatalIfErr(t, err)
				glog.Infof("Wrote %d bytes", n)
				testutil.FatalIfErr(t, f.Sync())

				var wg sync.WaitGroup
				wg.Add(2)
				go func() {
					defer wg.Done()
					logCountCheck()
				}()
				go func() {
					defer wg.Done()
					lineCountCheck()
				}()
				wg.Wait()
			}

			{

				logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
				lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

				logFile := path.Join(logDir, "log.2.txt")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
				testutil.FatalIfErr(t, err)
				glog.Infof("Wrote %d bytes", n)

				var wg sync.WaitGroup
				wg.Add(2)
				go func() {
					defer wg.Done()
					logCountCheck()
				}()
				go func() {
					defer wg.Done()
					lineCountCheck()
				}()
				wg.Wait()
			}
			{
				logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 0)
				lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

				logFile := path.Join(logDir, "log.2.txt")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
				testutil.FatalIfErr(t, err)
				glog.Infof("Wrote %d bytes", n)

				var wg sync.WaitGroup
				wg.Add(2)
				go func() {
					defer wg.Done()
					logCountCheck()
				}()
				go func() {
					defer wg.Done()
					lineCountCheck()
				}()
				wg.Wait()
			}

			glog.Infof("end")
		})
	}
}
