// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"os"
	"path"
	"testing"
	"time"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestTruncatedLogRead(t *testing.T) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)

	logFile := path.Join(logDir, "log")
	f := testutil.TestOpenFile(t, logFile)

	{
		linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)
		testutil.WriteString(t, f, "1\n")
		linesCountCheck()
	}
	err := f.Close()
	testutil.FatalIfErr(t, err)
	f, err = os.OpenFile(logFile, os.O_TRUNC|os.O_RDWR, 0600)
	testutil.FatalIfErr(t, err)
	{
		linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)
		// This magic sleep is long enough to wait for the test log watcher to poll again.
		time.Sleep(251 * time.Millisecond)
		testutil.WriteString(t, f, "2\n")
		linesCountCheck()
	}
	logCountCheck()
}
