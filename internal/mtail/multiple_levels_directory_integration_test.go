// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"sync"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestPollLogPathPatterns(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
	defer testutil.TestChdir(t, logDir)()

	// only manual polling -- zero for poll duration to avoid duplicates in test.
	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/files/*/log/*log"))
	defer stopM()

	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
	lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

	logFile := path.Join(logDir, "files", "a", "log", "a.log")
	testutil.FatalIfErr(t, os.MkdirAll(path.Dir(logFile), 0700))

	f := testutil.TestOpenFile(t, logFile)
	m.PollWatched()

	testutil.WriteString(t, f, "")
	m.PollWatched()

	testutil.WriteString(t, f, "line 1\n")
	m.PollWatched()

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
}
