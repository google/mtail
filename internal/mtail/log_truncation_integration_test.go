// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestLogTruncation(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))

	m, stopM := mtail.TestStartServer(t, 0, 1, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", 1)
	linesCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 2)

	logFile := filepath.Join(logDir, "log")
	f := testutil.TestOpenFile(t, logFile)
	m.PollWatched(1)

	testutil.WriteString(t, f, "line 1\n")
	m.PollWatched(1)
	// After the last barrier, the filestream may not race ahead of the test
	// here, so we need to ensure that a whole filestream loop occurs and that
	// the file offset advances for this test to succeed, hence the second
	// barrier here.
	m.PollWatched(1)

	err := f.Close()
	testutil.FatalIfErr(t, err)

	glog.Info("truncate")
	f, err = os.OpenFile(logFile, os.O_TRUNC|os.O_WRONLY, 0600)
	testutil.FatalIfErr(t, err)
	m.PollWatched(1)

	testutil.WriteString(t, f, "2\n")
	m.PollWatched(1)

	linesCountCheck()
	logCountCheck()
}
