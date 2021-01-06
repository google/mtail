// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestNewProg(t *testing.T) {
	testutil.SkipIfShort(t)

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0700)
	testutil.FatalIfErr(t, err)

	m, stopM := mtail.TestStartServer(t, 0, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	progLoadsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("prog_loads_total", "nocode.mtail", 1)

	testutil.TestOpenFile(t, progDir+"/nocode.mtail")
	m.PollWatched()

	progLoadsTotalCheck()
}

func TestProgramReloadNoDuplicateMetrics(t *testing.T) {
	testutil.SkipIfShort(t)

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	logDir := path.Join(workdir, "logs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0777))
	progDir := path.Join(workdir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0777))

	logFilepath := path.Join(logDir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 0, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	progLoadsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("prog_loads_total", "program.mtail", 1)

	progpath := path.Join(progDir, "program.mtail")
	p := testutil.TestOpenFile(t, progpath)
	testutil.WriteString(t, p, "counter foo\n/^foo$/ {\n foo++\n }\n")
	testutil.FatalIfErr(t, p.Close())
	m.PollWatched()

	progLoadsTotalCheck()

	fooIncreaseCheck := m.ExpectProgMetricDeltaWithDeadline("foo", 1)

	testutil.WriteString(t, logFile, "foo\n")
	m.PollWatched()

	fooIncreaseCheck()
	progLoadsTotalCheck = m.ExpectMapExpvarDeltaWithDeadline("prog_loads_total", "program.mtail", 1)

	p = testutil.TestOpenFile(t, progpath) // opens in append mode
	testutil.WriteString(t, p, "#\n")      // append just enough to change but still valid
	testutil.FatalIfErr(t, p.Close())
	m.PollWatched()

	progLoadsTotalCheck()

	// Should still be 1.
	fooIncreaseCheck()
}
