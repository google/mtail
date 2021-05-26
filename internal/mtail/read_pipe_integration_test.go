// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"net"
	"os"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
	"golang.org/x/sys/unix"
)

func TestReadFromPipe(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
	testutil.Chdir(t, logDir)

	logFile := filepath.Join(logDir, "logpipe")

	testutil.FatalIfErr(t, unix.Mkfifo(logFile, 0600))

	// TODO: race if this openfile happens after teststartserver.
	f, err := os.OpenFile(logFile, os.O_RDWR|syscall.O_NONBLOCK, 0600)
	testutil.FatalIfErr(t, err)
	defer func() {
		testutil.FatalIfErr(t, f.Close())
	}()

	m, stopM := mtail.TestStartServer(t, 1, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath(progDir))
	defer stopM()

	lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 3)

	testutil.WriteString(t, f, "1\n2\n3\n")
	m.PollWatched(0)

	lineCountCheck()
}

func TestReadFromSocket(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
	testutil.Chdir(t, logDir)

	logFile := filepath.Join(logDir, "sock")

	m, stopM := mtail.TestStartServer(t, 1, mtail.LogPathPatterns("unixgram://"+logDir+"/sock"), mtail.ProgramPath(progDir))
	defer stopM()

	lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 3)
	time.Sleep(10 * time.Millisecond)

	s, err := net.DialUnix("unixgram", nil, &net.UnixAddr{logFile, "unixgram"})
	testutil.FatalIfErr(t, err)
	defer func() {
		testutil.FatalIfErr(t, s.Close())
	}()

	_, err = s.Write([]byte("1\n2\n3\n"))
	testutil.FatalIfErr(t, err)

	m.PollWatched(0)

	lineCountCheck()
}
