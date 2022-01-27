// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestBasicTail(t *testing.T) {
	testutil.SkipIfShort(t)
	if testing.Verbose() {
		testutil.SetFlag(t, "vmodule", "tail=2,log_watcher=2")
	}
	logDir := testutil.TestTempDir(t)

	m, stopM := mtail.TestStartServer(t, 1, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
	defer stopM()

	logFile := filepath.Join(logDir, "log")

	lineCountCheck := m.ExpectMapExpvarDeltaWithDeadline("log_lines_total", logFile, 3)
	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", 1)

	f := testutil.TestOpenFile(t, logFile)
	defer f.Close()
	m.PollWatched(1) // Force sync to EOF

	for i := 1; i <= 3; i++ {
		testutil.WriteString(t, f, fmt.Sprintf("%d\n", i))
	}
	m.PollWatched(1) // Expect to read 3 lines here.

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
	workdir := testutil.TestTempDir(t)

	// Start mtail
	logFilepath := filepath.Join(workdir, "log")
	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(logFilepath))
	defer stopM()

	logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", 0)

	// touch log file
	newLogFilepath := filepath.Join(workdir, "log1")

	logFile, err := os.Create(newLogFilepath)
	testutil.FatalIfErr(t, err)
	defer logFile.Close()
	m.PollWatched(0) // No streams so don't wait for any.

	logCountCheck()
}
