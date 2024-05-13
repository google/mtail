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

func TestPartialLineRead(t *testing.T) {
	testutil.SkipIfShort(t)

	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0o700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0o700)
	testutil.FatalIfErr(t, err)

	logFile := filepath.Join(logDir, "log")

	f := testutil.TestOpenFile(t, logFile)
	defer f.Close()

	m, stopM := mtail.TestStartServer(t, 1, 1, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()
	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1) // Force read to EOF

	lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 2)

	testutil.WriteString(t, f, "line 1\n")
	m.AwakenLogStreams(1, 1)

	testutil.WriteString(t, f, "line ")
	// TODO: These PollLogPatterns should be unnecessary, but here are load-bearing.
	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1)

	testutil.WriteString(t, f, "2\n")
	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1)

	lineCountCheck()
}
