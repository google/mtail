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
	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0o700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0o700))

	m, stopM := mtail.TestStartServer(t, 1, 1, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", 1)
	linesCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 2)

	logFile := filepath.Join(logDir, "log")
	f := testutil.TestOpenFile(t, logFile)
	defer f.Close()
	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1) // Read to EOF

	testutil.WriteString(t, f, "line 1\n")
	m.AwakenLogStreams(1, 1)
	// After the last barrier, the filestream may not race ahead of the test
	// here, so we need to ensure that a whole filestream loop occurs and that
	// the file offset advances for this test to succeed, hence the second
	// barrier here.
	m.AwakenLogStreams(1, 1)

	err := f.Close()
	testutil.FatalIfErr(t, err)

	glog.Info("truncate")
	f, err = os.OpenFile(logFile, os.O_TRUNC|os.O_WRONLY, 0o600)
	testutil.FatalIfErr(t, err)
	defer f.Close()
	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1)

	testutil.WriteString(t, f, "2\n")
	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1)

	linesCountCheck()
	logCountCheck()
}
