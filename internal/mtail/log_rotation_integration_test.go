// +build integration

package mtail_test

import (
	"os"
	"path"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
)

func TestLogRotation(t *testing.T) {
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

	logFile := path.Join(logDir, "log")

	f, err := os.OpenFile(logFile, os.O_CREATE|os.O_RDWR|os.O_APPEND, 0600)
	if err != nil {
		t.Fatal(err)
	}

	m, stopM := mtail.TestStartServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir+"/log"))
	defer stopM()

	{
		n, err := f.WriteString("line 1\n")
		if err != nil {
			t.Fatal(err)
		}
		glog.Infof("Wrote %d bytes", n)
		time.Sleep(time.Second)
	}
	startLogLinesTotal := mtail.TestGetMetric(t, m.Addr(), "log_lines_total").(map[string]interface{})[logFile]

	{

		n, err := f.WriteString("line 2\n")
		if err != nil {
			t.Fatal(err)
		}
		glog.Infof("Wrote %d bytes", n)
		time.Sleep(time.Second)

		logLinesTotal := mtail.TestGetMetric(t, m.Addr(), "log_lines_total").(map[string]interface{})[logFile]

		mtail.ExpectMetricDelta(t, logLinesTotal, startLogLinesTotal, 1)
	}

	err = os.Rename(logFile, logFile+".1")
	if err != nil {
		t.Fatal(err)
	}

	f, err = os.OpenFile(logFile, os.O_CREATE|os.O_RDWR|os.O_APPEND, 0600)
	if err != nil {
		t.Fatal(err)
	}

	{
		n, err := f.WriteString("line 1\n")
		if err != nil {
			t.Fatal(err)
		}
		glog.Infof("Wrote %d bytes", n)
		time.Sleep(time.Second)

		logLinesTotal := mtail.TestGetMetric(t, m.Addr(), "log_lines_total").(map[string]interface{})[logFile]

		mtail.ExpectMetricDelta(t, logLinesTotal, startLogLinesTotal, 2)
	}
}
