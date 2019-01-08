// +build integration

package mtail_test

import (
	"os"
	"path"
	"testing"
	"time"

	"github.com/google/mtail/internal/mtail"
)

func TestNewProg(t *testing.T) {
	tmpDir, rmTmpDir := mtail.TestTempDir(t)
	defer rmTmpDir()

	logDir := path.Join(tmpDir, "logs")
	progDir := path.Join(tmpDir, "progs")
	err := os.Mkdir(logDir, 0700)
	if err != nil {
		t.Fatal(err)
	}
	err = os.Mkdir(progDir, 0700)
	if err != nil {
		t.Fatal(err)
	}

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/*"))
	defer stopM()

	startProgLoadsTotal := mtail.TestGetMetric(t, m.Addr(), "prog_loads_total").(map[string]interface{})

	_, err = os.OpenFile(progDir+"/nocode.mtail", os.O_CREATE|os.O_RDWR|os.O_APPEND, 0600)
	if err != nil {
		t.Fatal(err)
	}
	time.Sleep(time.Second)

	progLoadsTotal := mtail.TestGetMetric(t, m.Addr(), "prog_loads_total").(map[string]interface{})

	mtail.ExpectMetricDelta(t, progLoadsTotal["nocode.mtail"], startProgLoadsTotal["nocode.mtail"], 1)

}
