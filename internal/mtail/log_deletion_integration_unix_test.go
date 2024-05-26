// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix

package mtail_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

// TestLogDeletion is a unix-only test because on Windows files with open read handles cannot be deleted.
func TestLogDeletion(t *testing.T) {
	testutil.SkipIfShort(t)
	workdir := testutil.TestTempDir(t)

	// touch log file
	logFilepath := filepath.Join(workdir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 1, 1, mtail.LogPathPatterns(logFilepath))
	defer stopM()

	logCloseCheck := m.ExpectMapExpvarDeltaWithDeadline("log_closes_total", logFilepath, 1)
	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", -1)

	m.AwakenPatternPollers(1, 1)
	m.AwakenLogStreams(1, 1) // Force read to EOF

	glog.Info("remove")
	testutil.FatalIfErr(t, os.Remove(logFilepath))

	m.AwakenLogStreams(1, 0) // run stream to observe it's missing
	logCloseCheck()
	m.AwakenGcPoller(1, 1)
	logCountCheck()
}
