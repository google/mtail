// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"fmt"
	"os"
	"os/user"
	"path"
	"testing"
	"time"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestPermissionDeniedOnLog(t *testing.T) {
	u, err := user.Current()
	if err != nil {
		t.Skip(fmt.Sprintf("Couldn't determine current user id: %s", err))
	}
	if u.Uid == "0" {
		t.Skip("Skipping test when run as root")
	}

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err = os.Mkdir(logDir, 0700)
	if err != nil {
		t.Fatal(err)
	}
	err = os.Mkdir(progDir, 0700)
	if err != nil {
		t.Fatal(err)
	}

	logFile := path.Join(logDir, "log")

	// Hide the error from stdout during test.
	defer testutil.TestSetFlag(t, "stderrthreshold", "FATAL")()

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	startErrorsTotal := mtail.TestGetMetric(t, m.Addr(), "log_errors_total").(map[string]interface{})

	f, err := os.OpenFile(logFile, os.O_CREATE, 0)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	time.Sleep(time.Second)

	errorsTotal := mtail.TestGetMetric(t, m.Addr(), "log_errors_total").(map[string]interface{})

	mtail.ExpectMetricDelta(t, errorsTotal[logFile], startErrorsTotal[logFile], 1)
}
