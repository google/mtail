// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"os"
	"path"
	"sync"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestTruncatedLogRead(t *testing.T) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
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

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 2)
	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)

	logFile := path.Join(logDir, "log")
	f := testutil.TestOpenFile(t, logFile)
	time.Sleep(time.Second)

	n, err := f.WriteString("1\n")
	if err != nil {
		t.Fatal(err)
	}
	glog.Infof("Wrote %d bytes", n)
	time.Sleep(time.Second)
	err = f.Close()
	if err != nil {
		t.Fatal(err)
	}
	f, err = os.OpenFile(logFile, os.O_TRUNC|os.O_RDWR, 0600)
	if err != nil {
		t.Fatal(err)
	}
	time.Sleep(time.Second)
	n, err = f.WriteString("2\n")
	if err != nil {
		t.Fatal(err)
	}
	glog.Infof("Wrote %d bytes", n)

	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		linesCountCheck()
	}()
	go func() {
		defer wg.Done()
		logCountCheck()
	}()
	wg.Wait()
}
