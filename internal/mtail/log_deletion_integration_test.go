// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestLogDeletion(t *testing.T) {
	t.Skip("broken, was commented out")
	testutil.SkipIfShort(t)
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	// touch log file
	logFilepath := path.Join(workdir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 0, 1, mtail.LogPathPatterns(logFilepath))
	defer stopM()

	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", -1)

	glog.Info("remove")
	testutil.FatalIfErr(t, os.Remove(logFilepath))

	m.PollWatched(1) // one pass to stop
	// TODO(jaq): this sleep hides a race between filestream completing and
	// PollLogStreams noticing.
	time.Sleep(10 * time.Millisecond)
	m.PollWatched(0) // one pass to remove completed stream

	logCountCheck()
}
