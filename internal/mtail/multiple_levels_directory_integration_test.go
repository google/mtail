// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

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

	startLogCount := mtail.TestGetMetric(t, m.Addr(), "log_count")
	startLineCount := mtail.TestGetMetric(t, m.Addr(), "lines_total")

	logFile := path.Join(logDir, "files", "a", "log", "a.log")
	testutil.FatalIfErr(t, os.MkdirAll(path.Dir(logFile), 0700))
	f := testutil.TestOpenFile(t, logFile)
	n, err := f.WriteString("")
	testutil.FatalIfErr(t, err)
	time.Sleep(time.Second)
	f.WriteString("line 1\n")
	glog.Infof("Wrote %d bytes", n)
	time.Sleep(time.Second)

	logCount := mtail.TestGetMetric(t, m.Addr(), "log_count")
	lineCount := mtail.TestGetMetric(t, m.Addr(), "lines_total")

	if logCount.(float64)-startLogCount.(float64) != 1. {
		t.Errorf("Unexpected log count: got %g, want 1", logCount.(float64)-startLogCount.(float64))
	}
	if lineCount.(float64)-startLineCount.(float64) != 1. {
		t.Errorf("Unexpected line count: got %g, want 1", lineCount.(float64)-startLineCount.(float64))
	}
	time.Sleep(time.Second)

}
