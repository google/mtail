// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"fmt"
	"os"
	"path"
	"sync"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestBasicTail(t *testing.T) {
	testutil.SkipIfShort(t)
	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "tail=2,log_watcher=2")()
	}
	logDir, rmLogDir := testutil.TestTempDir(t)
	defer rmLogDir()

	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
	defer stopM()

	lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 3)
	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)

	logFile := path.Join(logDir, "log")

	f := testutil.TestOpenFile(t, logFile)
	m.PollWatched() // Force sync to EOF

	for i := 1; i <= 3; i++ {
		testutil.WriteString(t, f, fmt.Sprintf("%d\n", i))
	}
	m.PollWatched()

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

func TestNewLogDoesNotMatchIsIgnored(t *testing.T) {
	testutil.SkipIfShort(t)
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()
	// Start mtail
	logFilepath := path.Join(workdir, "log")
	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(logFilepath))
	defer stopM()

	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 0)

	// touch log file
	newLogFilepath := path.Join(workdir, "log1")

	logFile, err := os.Create(newLogFilepath)
	testutil.FatalIfErr(t, err)
	defer logFile.Close()
	m.PollWatched()

	logCountCheck()
}
