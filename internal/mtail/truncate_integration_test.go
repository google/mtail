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
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 2)
	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)

	logFile := path.Join(logDir, "log")
	f := testutil.TestOpenFile(t, logFile)
	time.Sleep(time.Second)

	n, err := f.WriteString("1\n")
	testutil.FatalIfErr(t, err)
	glog.Infof("Wrote %d bytes", n)
	time.Sleep(time.Second)
	err = f.Close()
	testutil.FatalIfErr(t, err)
	f, err = os.OpenFile(logFile, os.O_TRUNC|os.O_RDWR, 0600)
	testutil.FatalIfErr(t, err)
	time.Sleep(time.Second)
	n, err = f.WriteString("2\n")
	testutil.FatalIfErr(t, err)
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
