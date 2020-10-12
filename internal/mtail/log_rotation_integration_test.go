// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"fmt"
	"os"
	"path"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestLogRotation(t *testing.T) {
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

			logFile := path.Join(logDir, "log")

			f := testutil.TestOpenFile(t, logFile)

			m, stopM := mtail.TestStartServer(t, test.PollInterval, test.EnableFsNotify, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
			defer stopM()

			testutil.WriteString(t, f, "line 1\n")
			m.PollWatched()

			{
				logLinesTotalCheck := m.ExpectMapMetricDeltaWithDeadline("log_lines_total", logFile, 1)

				testutil.WriteString(t, f, "line 2\n")
				m.PollWatched()
				logLinesTotalCheck()
			}

			err = os.Rename(logFile, logFile+".1")
			testutil.FatalIfErr(t, err)
			m.PollWatched()

			f = testutil.TestOpenFile(t, logFile)

			{
				logLinesTotalCheck := m.ExpectMapMetricDeltaWithDeadline("log_lines_total", logFile, 1)

				testutil.WriteString(t, f, "line 1\n")
				m.PollWatched()
				logLinesTotalCheck()
			}
		})
	}
}
