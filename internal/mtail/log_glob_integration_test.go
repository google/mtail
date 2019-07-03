// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"fmt"
	"os"
	"path"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestLogGlobMatchesAfterStartupWithPollInterval(t *testing.T) {
	for _, pollInterval := range []time.Duration{0, 250 * time.Millisecond} {
		t.Run(fmt.Sprintf("%s", pollInterval), func(t *testing.T) {
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()

			logDir := path.Join(tmpDir, "logs")
			progDir := path.Join(tmpDir, "progs")
			testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
			testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
			defer testutil.TestChdir(t, logDir)()

			m, stopM := mtail.TestStartServer(t, pollInterval, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log*"))
			defer stopM()

			startLogCount := mtail.TestGetMetric(t, m.Addr(), "log_count")
			startLineCount := mtail.TestGetMetric(t, m.Addr(), "line_count")

			{
				logFile := path.Join(logDir, "log")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
				testutil.FatalIfErr(t, err)
				glog.Infof("Wrote %d bytes", n)
				time.Sleep(time.Second)

				logCount := mtail.TestGetMetric(t, m.Addr(), "log_count")
				lineCount := mtail.TestGetMetric(t, m.Addr(), "line_count")

				if logCount.(float64)-startLogCount.(float64) != 1. {
					t.Errorf("Unexpected log count: got %g, want 1", logCount.(float64)-startLogCount.(float64))
				}
				if lineCount.(float64)-startLineCount.(float64) != 1. {
					t.Errorf("Unexpected line count: got %g, want 1", lineCount.(float64)-startLineCount.(float64))
				}
				time.Sleep(time.Second)
			}
			{

				logFile := path.Join(logDir, "log1")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
				testutil.FatalIfErr(t, err)
				glog.Infof("Wrote %d bytes", n)
				time.Sleep(time.Second)

				logCount := mtail.TestGetMetric(t, m.Addr(), "log_count")
				lineCount := mtail.TestGetMetric(t, m.Addr(), "line_count")

				if logCount.(float64)-startLogCount.(float64) != 2. {
					t.Errorf("Unexpected log count: got %g, want 2", logCount.(float64)-startLogCount.(float64))
				}
				if lineCount.(float64)-startLineCount.(float64) != 2. {
					t.Errorf("Unexpected line count: got %g, want 2", lineCount.(float64)-startLineCount.(float64))
				}
				time.Sleep(time.Second)
			}
		})
	}
}
