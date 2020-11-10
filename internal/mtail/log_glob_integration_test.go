// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"fmt"
	"os"
	"path"
	"sync"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestLogGlobMatchesAfterStartupWithPollInterval(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	for _, test := range mtail.LogWatcherTestTable {
		t.Run(fmt.Sprintf("%s %v", test.PollInterval, test.EnableFsNotify), func(t *testing.T) {
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()

			logDir := path.Join(tmpDir, "logs")
			progDir := path.Join(tmpDir, "progs")
			testutil.FatalIfErr(t, os.Mkdir(logDir, 0700))
			testutil.FatalIfErr(t, os.Mkdir(progDir, 0700))
			defer testutil.TestChdir(t, logDir)()

			m, stopM := mtail.TestStartServer(t, test.PollInterval, test.EnableFsNotify, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log*"))
			defer stopM()

			{
				logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
				linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

				logFile := path.Join(logDir, "log")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
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
			{

				logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
				linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

				logFile := path.Join(logDir, "log1")
				f := testutil.TestOpenFile(t, logFile)
				n, err := f.WriteString("line 1\n")
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
		})
	}
}

func TestGlob(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	globTests := []struct {
		name     string
		expected bool
	}{
		{
			path.Join(workdir, "log1"),
			true,
		},
		{
			path.Join(workdir, "log2"),
			true,
		},
		{
			path.Join(workdir, "1log"),
			false,
		},
	}
	count := 0
	for _, tt := range globTests {
		log := testutil.TestOpenFile(t, tt.name)
		defer log.Close()
		if tt.expected {
			count++
		}
		testutil.WriteString(t, log, "\n")
	}
	m, stopM := mtail.TestStartServer(t, 0, true, mtail.LogPathPatterns(path.Join(workdir, "log*")))
	defer stopM()

	if r := m.GetMetric("log_count"); r != float64(count) {
		t.Errorf("Expecting log count of %d, received %g", count, r)
	}
}
