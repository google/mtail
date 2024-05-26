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

func TestMultipleLinesInOneWrite(t *testing.T) {
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

	{
		lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 1)
		n, err := f.WriteString("line 1\n")
		testutil.FatalIfErr(t, err)
		glog.Infof("Wrote %d bytes", n)
		m.AwakenLogStreams(1, 1)
		lineCountCheck()
	}

	{
		lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 2)
		n, err := f.WriteString("line 2\nline 3\n")
		testutil.FatalIfErr(t, err)
		glog.Infof("Wrote %d bytes", n)
		m.AwakenLogStreams(1, 1)
		lineCountCheck()
	}
}
