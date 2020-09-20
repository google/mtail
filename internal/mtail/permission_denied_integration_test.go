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

func TestPermissionDeniedOnLog(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0700)
	testutil.FatalIfErr(t, err)

	logFile := path.Join(logDir, "log")

	// Hide the error from stdout during test.
	defer testutil.TestSetFlag(t, "stderrthreshold", "FATAL")()

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	errorsTotalCheck := m.ExpectMapMetricDeltaWithDeadline("log_errors_total", logFile, 1)

	f, err := os.OpenFile(logFile, os.O_CREATE, 0)
	testutil.FatalIfErr(t, err)
	defer f.Close()

	errorsTotalCheck()
}
