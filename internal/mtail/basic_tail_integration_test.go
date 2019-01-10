// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"fmt"
	"path"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
)

func TestBasicTail(t *testing.T) {
	logDir, rmLogDir := mtail.TestTempDir(t)
	defer rmLogDir()

	//flag.Set("vmodule", "tail=2,log_watcher=2")
	m, stopM := mtail.TestStartServer(t, 0, false, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
	defer stopM()

	startLineCount := mtail.TestGetMetric(t, m.Addr(), "line_count")

	time.Sleep(1 * time.Second)

	logFile := path.Join(logDir, "log")

	f := mtail.TestOpenFile(t, logFile)

	for i := 1; i <= 3; i++ {
		n, err := f.WriteString(fmt.Sprintf("%d\n", i))
		if err != nil {
			t.Fatal(err)
		}
		glog.Infof("Wrote %d bytes", n)
		time.Sleep(1 * time.Second)
	}

	endLineCount := mtail.TestGetMetric(t, m.Addr(), "line_count")

	lineCount := endLineCount.(float64) - startLineCount.(float64)
	if lineCount != 3. {
		t.Errorf("output didn't have expected line count increase: want 3 got %#v", lineCount)
	}
}
