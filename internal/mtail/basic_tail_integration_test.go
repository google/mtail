// +build integration

package mtail_test

import (
	"bytes"
	"encoding/json"
	_ "expvar"
	"fmt"
	"net/http"
	"os"
	"path"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
)

func TestBasicTail(t *testing.T) {
	logDir, rmLogDir := tempDir(t)
	defer rmLogDir()

	//flag.Set("vmodule", "tail=2,log_watcher=2")
	m, stopM := StartServer(t, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
	defer stopM()

	time.Sleep(1 * time.Second)

	f, err := os.OpenFile(path.Join(logDir, "log"), os.O_CREATE|os.O_RDWR|os.O_APPEND, 0600)
	if err != nil {
		t.Fatal(err)
	}

	for i := 1; i <= 3; i++ {
		n, err := f.WriteString(fmt.Sprintf("%d\n", i))
		if err != nil {
			t.Fatal(err)
		}
		glog.Infof("Wrote %d bytes", n)
		time.Sleep(1 * time.Second)
	}

	uri := fmt.Sprintf("http://%s/debug/vars", m.Addr())
	resp, err := http.Get(uri)
	if err != nil {
		t.Fatal(err)
	}
	buf := new(bytes.Buffer)
	buf.ReadFrom(resp.Body)
	var r map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &r); err != nil {
		t.Fatal(err)
	}
	if r["line_count"] != 3. {
		t.Errorf("output didn't have expected line count increase: want 3 got %#v", r["line_count"])
		t.Log(buf.String())
	}
}
