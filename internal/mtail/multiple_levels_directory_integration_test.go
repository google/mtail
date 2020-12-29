// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestPollLogPathPatterns(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	defer testutil.TestChdir(t, logDir)()

	m, stopM := mtail.TestStartServer(t, 0, 0, mtail.LogPathPatterns(logDir+"/files/*/log/*log"))
	defer stopM()

	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
	lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

	logFile := path.Join(logDir, "files", "a", "log", "a.log")
	testutil.FatalIfErr(t, os.MkdirAll(path.Dir(logFile), 0700))
	m.PollWatched()

	f := testutil.TestOpenFile(t, logFile)
	m.PollWatched()

	logCountCheck()

	testutil.WriteString(t, f, "line 1\n")
	m.PollWatched()
	lineCountCheck()
}
