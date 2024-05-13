// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix

package mtail_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

// TestPermissionDeniedOnLog is a unix-specific test because on Windows, it is
// not possible to create a file that you yourself cannot read; there the
// minimum permissions are 0222.
func TestPermissionDeniedOnLog(t *testing.T) {
	testutil.SkipIfShort(t)
	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0o700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0o700)
	testutil.FatalIfErr(t, err)

	logFile := filepath.Join(logDir, "log")

	// Hide the error from stdout during test.
	testutil.SetFlag(t, "stderrthreshold", "FATAL")

	m, stopM := mtail.TestStartServer(t, 1, 0, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	errorsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_errors_total", logFile, 1)

	f, err := os.OpenFile(logFile, os.O_CREATE, 0)
	testutil.FatalIfErr(t, err)
	defer f.Close()

	// Nothing to await on, we expect to get a Permission Denied in the
	// synchronous logstream.New path.
	m.AwakenPatternPollers(1, 1)

	errorsTotalCheck()
}
