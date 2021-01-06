// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestHandleLogDeletes(t *testing.T) {
	t.Skip("broken, was commented out")
	testutil.SkipIfShort(t)
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	// touch log file
	logFilepath := path.Join(workdir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(logFilepath))
	defer stopM()

	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", -1)

	testutil.FatalIfErr(t, os.Remove(logFilepath))

	m.PollWatched()

	logCountCheck()
}
