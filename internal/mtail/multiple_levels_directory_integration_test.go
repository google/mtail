// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"path/filepath"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestPollLogPathPatterns(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.Chdir(t, logDir)

	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(logDir+"/files/*/log/*log"))
	defer stopM()

	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", 1)
	lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 1)

	logFile := filepath.Join(logDir, "files", "a", "log", "a.log")
	testutil.FatalIfErr(t, os.MkdirAll(path.Dir(logFile), 0700))

	f := testutil.TestOpenFile(t, logFile)
	m.PollWatched(1)

	logCountCheck()

	testutil.WriteString(t, f, "line 1\n")
	m.PollWatched(1)
	lineCountCheck()
}
