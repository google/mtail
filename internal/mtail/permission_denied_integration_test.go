// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestPermissionDeniedOnLog(t *testing.T) {
	testutil.SkipIfShort(t)
	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0700)
	testutil.FatalIfErr(t, err)

	logFile := filepath.Join(logDir, "log")

	// Hide the error from stdout during test.
	testutil.SetFlag(t, "stderrthreshold", "FATAL")

	m, stopM := mtail.TestStartServer(t, 0, 0, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	errorsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_errors_total", logFile, 1)

	f, err := os.OpenFile(logFile, os.O_CREATE, 0)
	testutil.FatalIfErr(t, err)
	defer f.Close()

	// Nothing to await on, we expect to get a Permission Denied in the
	// synchronous logstream.New path.
	m.PollWatched(0)

	errorsTotalCheck()
}
