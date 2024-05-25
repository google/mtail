// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix

package mtail_test

import (
	"net"
	"os"
	"path/filepath"
	"syscall"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
	"golang.org/x/sys/unix"
)

func TestReadFromPipe(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0o700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0o700))
	testutil.Chdir(t, logDir)

	logFile := filepath.Join(logDir, "logpipe")

	testutil.FatalIfErr(t, unix.Mkfifo(logFile, 0o600))

	// TODO: race if this openfile happens after teststartserver.
	f, err := os.OpenFile(logFile, os.O_RDWR|syscall.O_NONBLOCK, 0o600)
	testutil.FatalIfErr(t, err)
	defer func() {
		testutil.FatalIfErr(t, f.Close())
	}()

	m, stopM := mtail.TestStartServer(t, 1, 1, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath(progDir))
	defer stopM()

	lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 3)

	testutil.WriteString(t, f, "1\n2\n3\n")
	m.AwakenPatternPollers(1, 1)

	lineCountCheck()
}

func TestReadFromSocket(t *testing.T) {
	testutil.SkipIfShort(t)

	for _, scheme := range []string{"unix", "unixgram"} {
		scheme := scheme
		t.Run(scheme, func(t *testing.T) {
			tmpDir := testutil.TestTempDir(t)

			logDir := filepath.Join(tmpDir, "logs")
			progDir := filepath.Join(tmpDir, "progs")
			testutil.FatalIfErr(t, os.Mkdir(logDir, 0o700))
			testutil.FatalIfErr(t, os.Mkdir(progDir, 0o700))
			testutil.Chdir(t, logDir)

			logFile := filepath.Join(logDir, "sock")

			m, stopM := mtail.TestStartServer(t, 1, 1, mtail.LogPathPatterns(scheme+"://"+logDir+"/sock"), mtail.ProgramPath(progDir))
			defer stopM()

			lineCountCheck := m.ExpectExpvarDeltaWithDeadline("lines_total", 3)

			s, err := net.DialUnix(scheme, nil, &net.UnixAddr{Name: logFile, Net: scheme})
			testutil.FatalIfErr(t, err)
			defer func() {
				testutil.FatalIfErr(t, s.Close())
			}()

			_, err = s.Write([]byte("1\n2\n3\n"))
			testutil.FatalIfErr(t, err)

			lineCountCheck()
		})
	}
}
