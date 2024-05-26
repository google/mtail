// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix

package mtail_test

import (
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

// TestLogRotationByRename is a unix-specific test because on Windows, files
// cannot be removed or renamed while there is an open read handle on
// them. Instead, log rotation would have to be implemented by copying and then
// truncating the original file. That test case is already covered by
// TestLogTruncation.
func TestLogRotationByRename(t *testing.T) {
	testutil.SkipIfShort(t)

	for _, tc := range []bool{false, true} {
		tc := tc
		name := "disabled"
		if tc {
			name = "enabled"
		}
		t.Run(fmt.Sprintf("race simulation %s", name), func(t *testing.T) {
			tmpDir := testutil.TestTempDir(t)

			logDir := filepath.Join(tmpDir, "logs")
			progDir := filepath.Join(tmpDir, "progs")
			err := os.Mkdir(logDir, 0o700)
			testutil.FatalIfErr(t, err)
			err = os.Mkdir(progDir, 0o700)
			testutil.FatalIfErr(t, err)

			logFile := filepath.Join(logDir, "log")

			f := testutil.TestOpenFile(t, logFile)
			defer f.Close()

			m, stopM := mtail.TestStartServer(t, 1, 1, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
			defer stopM()

			logOpensTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_opens_total", logFile, 1)
			logLinesTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_lines_total", logFile, 3)

			testutil.WriteString(t, f, "line 1\n")
			m.AwakenLogStreams(1, 1)

			testutil.WriteString(t, f, "line 2\n")
			m.AwakenLogStreams(1, 1)

			logClosedCheck := m.ExpectMapExpvarDeltaWithDeadline("log_closes_total", logFile, 1)
			logCompletedCheck := m.ExpectExpvarDeltaWithDeadline("log_count", -1)
			glog.Info("rename")
			err = os.Rename(logFile, logFile+".1")
			testutil.FatalIfErr(t, err)
			if tc {
				// Simulate a race where we poll for a pattern and remove the
				// existing stream.
				m.AwakenPatternPollers(1, 1) // simulate race condition with this poll.
				m.AwakenLogStreams(1, 0)
				logClosedCheck() // barrier until filestream closes fd
				m.AwakenGcPoller(1, 1)
				logCompletedCheck() // barrier until the logstream is removed from tailer
			}
			glog.Info("create")
			f = testutil.TestOpenFile(t, logFile)
			m.AwakenPatternPollers(1, 1)
			m.AwakenLogStreams(0, 1)
			testutil.WriteString(t, f, "line 1\n")
			m.AwakenLogStreams(1, 1)

			var wg sync.WaitGroup
			wg.Add(2)
			go func() {
				defer wg.Done()
				logLinesTotalCheck()
			}()
			go func() {
				defer wg.Done()

				logOpensTotalCheck()
			}()
			wg.Wait()
		})
	}
}
