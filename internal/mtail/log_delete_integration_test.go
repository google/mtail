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

func TestHandleLogDeletes(t *testing.T) {
	testutil.SkipIfShort(t)
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	// touch log file
	logFilepath := path.Join(workdir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 0, 0, mtail.LogPathPatterns(logFilepath))
	defer stopM()

	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", -1)
	m.PollWatched()

	glog.Info("remove")
	testutil.FatalIfErr(t, os.Remove(logFilepath))

	m.PollWatched()                   // one pass to stop
	time.Sleep(20 * time.Millisecond) // TODO(jaq): fix synchronisation of logstream.Stop()
	m.PollWatched()                   // one pass to remove

	logCountCheck()
}
