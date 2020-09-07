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

func TestLogRotation(t *testing.T) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	if err != nil {
		t.Fatal(err)
	}
	err = os.Mkdir(progDir, 0700)
	if err != nil {
		t.Fatal(err)
	}

	logFile := path.Join(logDir, "log")

	f := testutil.TestOpenFile(t, logFile)

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	testutil.WriteString(t, f, "line 1\n")
	time.Sleep(1 * time.Second)

	{
		logLinesTotalCheck := mtail.ExpectMapMetricDeltaWithDeadline(t, m.Addr(), "log_lines_total", logFile, 1, time.Minute)

		testutil.WriteString(t, f, "line 2\n")
		time.Sleep(1 * time.Second)
		logLinesTotalCheck()
	}

	err = os.Rename(logFile, logFile+".1")
	if err != nil {
		t.Fatal(err)
	}

	f = testutil.TestOpenFile(t, logFile)

	{
		logLinesTotalCheck := mtail.ExpectMapMetricDeltaWithDeadline(t, m.Addr(), "log_lines_total", logFile, 1, time.Minute)

		testutil.WriteString(t, f, "line 1\n")
		time.Sleep(1 * time.Second)
		logLinesTotalCheck()
	}
}
