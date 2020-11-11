// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"sync"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestSoftLinkChange(t *testing.T) {
	testutil.SkipIfShort(t)
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	logFilepath := path.Join(workdir, "log")

	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(logFilepath))
	defer stopM()

	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
	logRotationsTotalCheck := m.ExpectMapMetricDeltaWithDeadline("log_rotations_total", logFilepath, 1)

	trueLog1 := testutil.TestOpenFile(t, logFilepath+".true1")
	defer trueLog1.Close()

	testutil.FatalIfErr(t, os.Symlink(logFilepath+".true1", logFilepath))
	glog.Info("symlinked")
	m.PollWatched()

	inputLines := []string{"hi1", "hi2", "hi3"}
	for _, x := range inputLines {
		testutil.WriteString(t, trueLog1, x+"\n")
	}
	m.PollWatched()

	trueLog2 := testutil.TestOpenFile(t, logFilepath+".true2")
	defer trueLog2.Close()
	m.PollWatched()

	testutil.FatalIfErr(t, os.Remove(logFilepath))
	m.PollWatched()
	testutil.FatalIfErr(t, os.Symlink(logFilepath+".true2", logFilepath))
	m.PollWatched()

	for _, x := range inputLines {
		testutil.WriteString(t, trueLog2, x+"\n")
	}
	m.PollWatched()

	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		logCountCheck()
	}()
	go func() {
		defer wg.Done()
		logRotationsTotalCheck()
	}()
	wg.Wait()

	_, err := os.Stat(logFilepath + ".true1")
	testutil.FatalIfErr(t, err)
	_, err = os.Stat(logFilepath + ".true2")
	testutil.FatalIfErr(t, err)
}
