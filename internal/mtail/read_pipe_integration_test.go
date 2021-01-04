// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"syscall"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
	"golang.org/x/sys/unix"
)

func TestReadFromPipe(t *testing.T) {
	t.Skip("flaky, pipes don't sync to EOF with pollwatched")
	testutil.SkipIfShort(t)
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
	defer testutil.TestChdir(t, logDir)()

	logFile := path.Join(logDir, "logpipe")

	testutil.FatalIfErr(t, unix.Mkfifo(logFile, 0600))

	f, err := os.OpenFile(logFile, os.O_RDWR|syscall.O_NONBLOCK, 0600)
	testutil.FatalIfErr(t, err)
	defer func() {
		testutil.FatalIfErr(t, f.Close())
	}()

	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath(progDir))
	defer stopM()

	lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 3)

	testutil.WriteString(t, f, "1\n2\n3\n")
	m.PollWatched()

	lineCountCheck()
}
