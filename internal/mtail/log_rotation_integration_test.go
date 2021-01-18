// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

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

func TestLogRotation(t *testing.T) {
	testutil.SkipIfShort(t)

	for _, tc := range []bool{false, true} {
		name := "disabled"
		if tc {
			name = "enabled"
		}
		t.Run(fmt.Sprintf("race simulation %s", name), func(t *testing.T) {
			tmpDir := testutil.TestTempDir(t)

			logDir := filepath.Join(tmpDir, "logs")
			progDir := filepath.Join(tmpDir, "progs")
			err := os.Mkdir(logDir, 0700)
			testutil.FatalIfErr(t, err)
			err = os.Mkdir(progDir, 0700)
			testutil.FatalIfErr(t, err)

			logFile := filepath.Join(logDir, "log")

			f := testutil.TestOpenFile(t, logFile)

			m, stopM := mtail.TestStartServer(t, 0, 1, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
			defer stopM()

			logOpensTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_opens_total", logFile, 1)
			logLinesTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_lines_total", logFile, 3)

			testutil.WriteString(t, f, "line 1\n")
			m.PollWatched(1)

			testutil.WriteString(t, f, "line 2\n")
			m.PollWatched(1)

			logClosedCheck := m.ExpectMapExpvarDeltaWithDeadline("log_closes_total", logFile, 1)
			logCompletedCheck := m.ExpectExpvarDeltaWithDeadline("log_count", -1)
			glog.Info("rename")
			err = os.Rename(logFile, logFile+".1")
			testutil.FatalIfErr(t, err)
			if tc {
				m.PollWatched(0)    // simulate race condition with this poll.
				logClosedCheck()    // sync when filestream closes fd
				m.PollWatched(0)    // invoke the GC
				logCompletedCheck() // sync to when the logstream is removed from tailer
			}
			glog.Info("create")
			f = testutil.TestOpenFile(t, logFile)
			m.PollWatched(1)
			testutil.WriteString(t, f, "line 1\n")
			m.PollWatched(1)

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

func TestLogSoftLinkChange(t *testing.T) {
	testutil.SkipIfShort(t)

	for _, tc := range []bool{false, true} {
		name := "disabled"
		if tc {
			name = "enabled"
		}
		t.Run(fmt.Sprintf("race simulation %s", name), func(t *testing.T) {
			workdir := testutil.TestTempDir(t)

			logFilepath := filepath.Join(workdir, "log")

			m, stopM := mtail.TestStartServer(t, 0, 1, mtail.LogPathPatterns(logFilepath))
			defer stopM()

			logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", 1)
			logOpensTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_opens_total", logFilepath, 2)

			trueLog1 := testutil.TestOpenFile(t, logFilepath+".true1")
			defer trueLog1.Close()

			testutil.FatalIfErr(t, os.Symlink(logFilepath+".true1", logFilepath))
			glog.Info("symlinked")
			m.PollWatched(1)

			inputLines := []string{"hi1", "hi2", "hi3"}
			for _, x := range inputLines {
				testutil.WriteString(t, trueLog1, x+"\n")
			}
			m.PollWatched(1)

			trueLog2 := testutil.TestOpenFile(t, logFilepath+".true2")
			defer trueLog2.Close()
			m.PollWatched(1)
			logClosedCheck := m.ExpectMapExpvarDeltaWithDeadline("log_closes_total", logFilepath, 1)
			logCompletedCheck := m.ExpectExpvarDeltaWithDeadline("log_count", -1)
			testutil.FatalIfErr(t, os.Remove(logFilepath))
			if tc {
				m.PollWatched(0)    // simulate race condition with this poll.
				logClosedCheck()    // sync when filestream closes fd
				m.PollWatched(0)    // invoke the GC
				logCompletedCheck() // sync to when the logstream is removed from tailer
			}
			testutil.FatalIfErr(t, os.Symlink(logFilepath+".true2", logFilepath))
			m.PollWatched(1)

			for _, x := range inputLines {
				testutil.WriteString(t, trueLog2, x+"\n")
			}
			m.PollWatched(1)

			var wg sync.WaitGroup
			wg.Add(2)
			go func() {
				defer wg.Done()
				logCountCheck()
			}()
			go func() {
				defer wg.Done()
				logOpensTotalCheck()
			}()
			wg.Wait()

			_, err := os.Stat(logFilepath + ".true1")
			testutil.FatalIfErr(t, err)
			_, err = os.Stat(logFilepath + ".true2")
			testutil.FatalIfErr(t, err)
		})
	}
}
