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

func TestProgramReloadNoDuplicateMetrics(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in shor tmode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	logDir := path.Join(workdir, "logs")
	testutil.FatalIfErr(t, os.Mkdir(logDir, 0777))
	progDir := path.Join(workdir, "progs")
	testutil.FatalIfErr(t, os.Mkdir(progDir, 0777))

	logFilepath := path.Join(logDir, "log")
	logFile := testutil.TestOpenFile(t, logFilepath)
	defer logFile.Close()

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	progLoadsTotalCheck := m.ExpectMapMetricDeltaWithDeadline("prog_loads_total", "program.mtail", 1)

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
	progLoadsTotalCheck = m.ExpectMapMetricDeltaWithDeadline("prog_loads_total", "program.mtail", 1)

	p = testutil.TestOpenFile(t, progpath) // opens in append mode
	testutil.WriteString(t, p, "#\n")      // append just enough to change but still valid
	testutil.FatalIfErr(t, p.Close())
	m.PollWatched()

	progLoadsTotalCheck()

	// Should still be 1.
	fooIncreaseCheck()
}
