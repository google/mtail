// Copyright 2019 Google Inc. All Rights Reserved.
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

func TestGlobBeforeStart(t *testing.T) {
	testutil.SkipIfShort(t)

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
	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(path.Join(workdir, "log*")))
	defer stopM()

	if r := m.GetMetric("log_count"); r != float64(count) {
		t.Errorf("Expecting log count of %d, received %g", count, r)
	}
}

func TestGlobAfterStart(t *testing.T) {
	testutil.SkipIfShort(t)

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
	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(path.Join(workdir, "log*")))
	defer stopM()

	count := 0
	for _, tt := range globTests {
		if tt.expected {
			count++
		}
	}
	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", float64(count))
	linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", float64(count))
	for _, tt := range globTests {
		log := testutil.TestOpenFile(t, tt.name)
		defer log.Close()
		testutil.WriteString(t, log, "\n")
	}
	m.PollWatched()
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

func TestGlobIgnoreFolder(t *testing.T) {
	testutil.SkipIfShort(t)

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	globTests := []struct {
		name     string
		isFolder bool
		expected bool
	}{
		{
			path.Join(workdir, "log1"),
			false,
			true,
		},
		{
			path.Join(workdir, "logarchive"),
			true,
			false,
		},
		{
			path.Join(workdir, "log2.gz"),
			false,
			false,
		},
	}
	count := 0
	for _, tt := range globTests {
		var err error
		var log *os.File

		if tt.isFolder {
			err = os.Mkdir(tt.name, 0700)
			testutil.FatalIfErr(t, err)
			continue
		} else {
			log, err = os.Create(tt.name)
		}

		if !tt.isFolder && tt.expected {
			count++
		}
		defer log.Close()
		testutil.FatalIfErr(t, err)
		testutil.WriteString(t, log, "\n")
	}
	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(path.Join(workdir, "log*")), mtail.IgnoreRegexPattern("\\.gz"))
	defer stopM()

	if r := m.GetMetric("log_count"); r != float64(count) {
		t.Errorf("Expecting log Count for %d, received %g", count, r)
	}
}

func TestFilenameRegexIgnore(t *testing.T) {
	testutil.SkipIfShort(t)

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
			path.Join(workdir, "log1.gz"),
			false,
		},
		{
			path.Join(workdir, "log2gz"),
			true,
		},
	}
	count := 0
	for _, tt := range globTests {
		log, err := os.Create(tt.name)
		testutil.FatalIfErr(t, err)
		defer log.Close()
		if tt.expected {
			count++
		}
		testutil.WriteString(t, log, "\n")
	}

	m, stopM := mtail.TestStartServer(t, 0, mtail.LogPathPatterns(path.Join(workdir, "log*")), mtail.IgnoreRegexPattern("\\.gz"))
	defer stopM()

	if r := m.GetMetric("log_count"); r != float64(count) {
		t.Errorf("Log count not matching\n\texpected: %d\n\t: received: %g", count, r)
	}
}

func TestGlobRelativeAfterStart(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0700)
	testutil.FatalIfErr(t, err)

	// Move to logdir to make relative paths
	defer testutil.TestChdir(t, logDir)()

	m, stopM := mtail.TestStartServer(t, 0, mtail.ProgramPath(progDir), mtail.LogPathPatterns("log.*"))
	defer stopM()

	{
		logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
		lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

		logFile := path.Join(logDir, "log.1.txt")
		f := testutil.TestOpenFile(t, logFile)

		testutil.WriteString(t, f, "line 1\n")
		m.PollWatched()

		var wg sync.WaitGroup
		wg.Add(2)
		go func() {
			defer wg.Done()
			logCountCheck()
		}()
		go func() {
			defer wg.Done()
			lineCountCheck()
		}()
		wg.Wait()
	}

	{

		logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 1)
		lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

		logFile := path.Join(logDir, "log.2.txt")
		f := testutil.TestOpenFile(t, logFile)
		testutil.WriteString(t, f, "line 1\n")
		m.PollWatched()

		var wg sync.WaitGroup
		wg.Add(2)
		go func() {
			defer wg.Done()
			logCountCheck()
		}()
		go func() {
			defer wg.Done()
			lineCountCheck()
		}()
		wg.Wait()
	}
	{
		logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", 0)
		lineCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", 1)

		logFile := path.Join(logDir, "log.2.txt")
		f := testutil.TestOpenFile(t, logFile)
		testutil.WriteString(t, f, "line 1\n")
		m.PollWatched()

		var wg sync.WaitGroup
		wg.Add(2)
		go func() {
			defer wg.Done()
			logCountCheck()
		}()
		go func() {
			defer wg.Done()
			lineCountCheck()
		}()
		wg.Wait()
	}

	glog.Infof("end")
}
