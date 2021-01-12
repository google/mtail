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

func TestPartialLineRead(t *testing.T) {
	testutil.SkipIfShort(t)

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

	m, stopM := mtail.TestStartServer(t, 0, 1, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 2)

	testutil.WriteString(t, f, "line 1\n")
	m.PollWatched(1)

	testutil.WriteString(t, f, "line ")
	m.PollWatched(1)

	testutil.WriteString(t, f, "2\n")
	m.PollWatched(1)

	lineCountCheck()
}
