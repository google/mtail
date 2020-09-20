// Copyright 2019 Google Inc. All Rights Reserved.
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
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	testutil.FatalIfErr(t, err)
	err = os.Mkdir(progDir, 0700)
	testutil.FatalIfErr(t, err)

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	progLoadsTotalCheck := m.ExpectMapMetricDeltaWithDeadline("prog_loads_total", "nocode.mtail", 1)

	testutil.TestOpenFile(t, progDir+"/nocode.mtail")

	progLoadsTotalCheck()
}
