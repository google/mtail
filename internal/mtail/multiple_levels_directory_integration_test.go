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

func TestPollLogPathPatterns(t *testing.T) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
	defer testutil.TestChdir(t, logDir)()

	m, stopM := mtail.TestStartServer(t, 10*time.Millisecond, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/files/*/log/*log"))
	defer stopM()

	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1, time.Minute)
	lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1, time.Minute)

	logFile := path.Join(logDir, "files", "a", "log", "a.log")
	testutil.FatalIfErr(t, os.MkdirAll(path.Dir(logFile), 0700))
	f := testutil.TestOpenFile(t, logFile)
	n, err := f.WriteString("")
	testutil.FatalIfErr(t, err)
	time.Sleep(time.Second)
	f.WriteString("line 1\n")
	glog.Infof("Wrote %d bytes", n)
	time.Sleep(time.Second)

	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		lineCountCheck()
	}()
	go func() {
		defer wg.Done()
		logCountCheck()
	}()
	wg.Wait()
}
