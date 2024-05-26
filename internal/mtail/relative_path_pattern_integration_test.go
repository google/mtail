// Copyright 2020 Google Inc. All Rights Reserved.
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

func TestRelativeLog(t *testing.T) {
	testutil.SkipIfShort(t)
	workdir := testutil.TestTempDir(t)

	cwd, err := os.Getwd()
	testutil.FatalIfErr(t, err)
	glog.Infof("cwd is %q", cwd)

	testutil.FatalIfErr(t, os.Chdir(workdir))
	defer func() {
		testutil.FatalIfErr(t, os.Chdir(cwd))
	}()

	// touch log file
	logFilepath := filepath.Join(workdir, "log")
	logFile, err := os.Create(logFilepath)
	testutil.FatalIfErr(t, err)
	defer logFile.Close()
	pathnames := []string{"log"}
	m, stopM := mtail.TestStartServer(t, 1, 1, mtail.LogPathPatterns(pathnames...))
	defer stopM()

	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1) // Force read to EOF

	inputLines := []string{"hi", "hi2", "hi3"}
	lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", int64(len(inputLines)))

	for _, x := range inputLines {
		// write to log file
		testutil.WriteString(t, logFile, x+"\n")
	}
	m.AwakenLogStreams(1, 1)

	lineCountCheck()
}
