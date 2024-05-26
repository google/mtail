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

func TestLogRotationBySoftLinkChange(t *testing.T) {
	testutil.SkipIfShort(t)

	for _, tc := range []bool{false, true} {
		tc := tc
		name := "disabled"
		if tc {
			name = "enabled"
		}
		t.Run(fmt.Sprintf("race simulation %s", name), func(t *testing.T) {
			workdir := testutil.TestTempDir(t)

			logFilepath := filepath.Join(workdir, "log")

			m, stopM := mtail.TestStartServer(t, 1, 1, mtail.LogPathPatterns(logFilepath))
			defer stopM()

			logCountCheck := m.ExpectExpvarDeltaWithDeadline("log_count", 1)
			logOpensTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("log_opens_total", logFilepath, 2)

			trueLog1 := testutil.TestOpenFile(t, logFilepath+".true1")
			defer trueLog1.Close()

			testutil.FatalIfErr(t, os.Symlink(logFilepath+".true1", logFilepath))
			glog.Info("symlinked")
			m.AwakenPatternPollers(1, 1)
			m.AwakenLogStreams(1, 1)

			inputLines := []string{"hi1", "hi2", "hi3"}
			for _, x := range inputLines {
				testutil.WriteString(t, trueLog1, x+"\n")
			}
			m.AwakenPatternPollers(1, 1)
			m.AwakenLogStreams(1, 1)

			trueLog2 := testutil.TestOpenFile(t, logFilepath+".true2")
			defer trueLog2.Close()
			m.AwakenPatternPollers(1, 1)
			m.AwakenLogStreams(1, 1)
			m.AwakenGcPoller(1, 1)
			logClosedCheck := m.ExpectMapExpvarDeltaWithDeadline("log_closes_total", logFilepath, 1)
			logCompletedCheck := m.ExpectExpvarDeltaWithDeadline("log_count", -1)
			testutil.FatalIfErr(t, os.Remove(logFilepath))
			if tc {
				// Simulate a race where we poll for a pattern and remove the
				// existing stream.
				m.AwakenPatternPollers(1, 1) // simulate race condition with this poll.
				m.AwakenLogStreams(1, 0)
				logClosedCheck() // barrier until filestream closes fd
				m.AwakenGcPoller(1, 1)
				logCompletedCheck() // barrier until the logstream is removed from tailer
			}
			testutil.FatalIfErr(t, os.Symlink(logFilepath+".true2", logFilepath))
			m.AwakenPatternPollers(1, 1)
			m.AwakenLogStreams(0, 1)

			for _, x := range inputLines {
				testutil.WriteString(t, trueLog2, x+"\n")
			}
			m.AwakenPatternPollers(1, 1)
			m.AwakenLogStreams(1, 1)

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
