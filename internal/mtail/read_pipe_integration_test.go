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
)

func TestReadFromPipe(t *testing.T) {
	tmpDir, rmTmpDir := mtail.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	if err != nil {
		t.Fatal(err)
	}
	err = os.Mkdir(progDir, 0700)
	if err != nil {
		t.Fatal(err)
	}
	defer mtail.TestChdir(t, logDir)()

	logFile := path.Join(logDir, "logpipe")

	err = syscall.Mkfifo(logFile, 0600)
	if err != nil {
		t.Fatal(err)
	}
	time.Sleep(time.Second)

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath(progDir))

	defer stopM()

	// TODO(jaq): Want to open the file before the startserver to the pipe
	// exists before start, but that hangs the server.
	f, err := os.OpenFile(logFile, os.O_RDWR|syscall.O_NONBLOCK, 0600)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		// TODO(jaq): Use f.SetDeadline once we stop using afero.
		err = f.Close()
		if err != nil {
			t.Fatal(err)
		}
	}()

	time.Sleep(time.Second)

	startLineCount := mtail.TestGetMetric(t, m.Addr(), "line_count")
	time.Sleep(1 * time.Second)

	n, err := f.WriteString("1\n2\n3\n")
	if err != nil {
		t.Fatal(err)
	}
	glog.Infof("Wrote %d bytes", n)
	time.Sleep(1 * time.Second)

	endLineCount := mtail.TestGetMetric(t, m.Addr(), "line_count")
	lineCount := endLineCount.(float64) - startLineCount.(float64)
	if lineCount != 3. {
		t.Errorf("output didn't have expected line count increase: want 3 got %#v", lineCount)
	}
}
