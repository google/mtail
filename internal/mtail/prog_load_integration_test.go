// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestNewProg(t *testing.T) {
	testutil.SkipIfShort(t)

	tmpDir := testutil.TestTempDir(t)

	logDir := filepath.Join(tmpDir, "logs")
	progDir := filepath.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0o700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0o700)
	testutil.FatalIfErr(t, err)

	m, stopM := mtail.TestStartServer(t, 0, 0, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	progLoadsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("prog_loads_total", "nocode.mtail", 1)

	f := testutil.TestOpenFile(t, progDir+"/nocode.mtail")
	defer f.Close()
	m.LoadAllPrograms()

	progLoadsTotalCheck()
}

func TestProgramReloadNoDuplicateMetrics(t *testing.T) {
	testutil.SkipIfShort(t)

	workdir := testutil.TestTempDir(t)

	logDir := filepath.Join(workdir, "logs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0o777))
	progDir := filepath.Join(workdir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0o777))

	logFilepath := filepath.Join(logDir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 1, 1, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	progLoadsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("prog_loads_total", "program.mtail", 1)

	progpath := filepath.Join(progDir, "program.mtail")
	p := testutil.TestOpenFile(t, progpath)
	testutil.WriteString(t, p, "counter foo\n/^foo$/ {\n foo++\n }\n")
	testutil.FatalIfErr(t, p.Close())
	m.LoadAllPrograms()

	progLoadsTotalCheck()

	fooIncreaseCheck := m.ExpectProgMetricDeltaWithDeadline("foo", "program.mtail", 1)

	testutil.WriteString(t, logFile, "foo\n")
	m.AwakenLogStreams(1, 1)

	fooIncreaseCheck()
	progLoadsTotalCheck = m.ExpectMapExpvarDeltaWithDeadline("prog_loads_total", "program.mtail", 1)

	p = testutil.TestOpenFile(t, progpath) // opens in append mode
	testutil.WriteString(t, p, "#\n")      // append just enough to change but still valid
	testutil.FatalIfErr(t, p.Close())
	m.LoadAllPrograms()

	progLoadsTotalCheck()

	// Should still be 1.
	fooIncreaseCheck()
}

func TestProgramUnloadIfDeleted(t *testing.T) {
	testutil.SkipIfShort(t)

	workdir := testutil.TestTempDir(t)

	logDir := filepath.Join(workdir, "logs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0o777))
	progDir := filepath.Join(workdir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0o777))

	logFilepath := filepath.Join(logDir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 0, 0, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	progLoadsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("prog_loads_total", "program.mtail", 1)

	progpath := filepath.Join(progDir, "program.mtail")
	p := testutil.TestOpenFile(t, progpath)
	testutil.WriteString(t, p, "counter foo\n/^foo$/ {\n foo++\n }\n")
	testutil.FatalIfErr(t, p.Close())
	m.LoadAllPrograms()

	progLoadsTotalCheck()

	progUnloadsTotalCheck := m.ExpectMapExpvarDeltaWithDeadline("prog_unloads_total", "program.mtail", 1)

	testutil.FatalIfErr(t, os.Remove(progpath))
	m.LoadAllPrograms()

	progUnloadsTotalCheck()
}
