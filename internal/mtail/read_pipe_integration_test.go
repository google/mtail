// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"os"
	"path"
	"syscall"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
	"golang.org/x/sys/unix"
)

func TestReadFromPipe(t *testing.T) {
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

	time.Sleep(time.Second)

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath(progDir))
	defer stopM()
	time.Sleep(time.Second)

	startLineCount := mtail.TestGetMetric(t, m.Addr(), "lines_total")
	time.Sleep(1 * time.Second)

	n, err := f.WriteString("1\n2\n3\n")
	testutil.FatalIfErr(t, err)
	glog.Infof("Wrote %d bytes", n)
	time.Sleep(1 * time.Second)

	endLineCount := mtail.TestGetMetric(t, m.Addr(), "lines_total")
	lineCount := endLineCount.(float64) - startLineCount.(float64)
	if lineCount != 3. {
		t.Errorf("output didn't have expected line count increase: want 3 got %#v", lineCount)
	}
}
