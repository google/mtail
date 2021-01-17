// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

// makeServer makes a new Server for use in tests, but does not start
// the server. It returns the server, or any errors the new server creates.
func makeServer(tb testing.TB, pollInterval time.Duration, options ...mtail.Option) (*mtail.Server, error) {
	tb.Helper()
	ctx := context.Background()

	return mtail.New(ctx, metrics.NewStore(), options...)
}

// startUNIXSocketServer creates a new Server serving through a UNIX
// socket and starts it running. It returns the server, and a cleanup function.
// startUNIXSocketServer differs from TestStartServer in that it uses UNIX sockets
// (see the usage of mtail.BindUnixSocket) instead of TCP sockets.
func startUNIXSocketServer(tb testing.TB, pollInterval time.Duration, options ...mtail.Option) (*mtail.Server, func()) {
	tb.Helper()

	tmpDir, rmTmpDir := testutil.TestTempDir(tb)

	unixSocket := filepath.Join(tmpDir, "mtail_test.socket")
	options = append(options, mtail.BindUnixSocket(unixSocket))

	m, err := makeServer(tb, pollInterval, options...)
	testutil.FatalIfErr(tb, err)

	errc := make(chan error, 1)
	go func() {
		err := m.Run()
		errc <- err
	}()

	glog.Infof("check that server is listening")

	addr, err := net.ResolveUnixAddr("unix", unixSocket)
	testutil.FatalIfErr(tb, err)
	_, err = net.DialUnix("unix", nil, addr)
	testutil.FatalIfErr(tb, err)

	return m, func() {
		defer rmTmpDir()
		select {
		case err := <-errc:
			testutil.FatalIfErr(tb, err)
		case <-time.After(5 * time.Second):
			buf := make([]byte, 1<<16)
			n := runtime.Stack(buf, true)
			fmt.Fprintf(os.Stderr, "%s", buf[0:n])
			tb.Fatal("timeout waiting for shutdown")
		}
	}
}

// getMetricFromUNIXSocket fetches the name metrics from the Server serving through
// the UNIX socket sockPath, and returns the value of one named name.  Callers are
// responsible for type assertions on the returned value.
// TODO: move this method to testing.go in order to abstract away the http.Transport part from TestGetMetric
func getMetricFromUNIXSocket(tb testing.TB, sockPath, name string) interface{} {
	uri := "http://unix/debug/vars"
	client := &http.Client{
		Transport: &http.Transport{
			DialContext: func(_ context.Context, _, _ string) (net.Conn, error) {
				return net.Dial("unix", sockPath)
			},
		},
		Timeout: 5 * time.Second,
	}
	resp, err := client.Get(uri)
	if err != nil {
		tb.Fatal(err)
	}
	buf := new(bytes.Buffer)
	n, err := buf.ReadFrom(resp.Body)
	testutil.FatalIfErr(tb, err)
	glog.V(2).Infof("TestGetMetric: http client read %d bytes from debug/vars", n)
	var r map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &r); err != nil {
		tb.Fatalf("%s: body was %s", err, buf.String())
	}
	glog.Infof("TestGetMetric: returned value for %s: %v", name, r[name])
	return r[name]
}

// expectMetricDelta checks to see if the difference between a and b is want;
// it assumes both values are float64s that came from a TestGetMetric.
func expectMetricDelta(tb testing.TB, a, b interface{}, want float64) {
	tb.Helper()
	if a == nil {
		a = 0.
	}
	if b == nil {
		b = 0.
	}
	delta := a.(float64) - b.(float64)
	if delta != want {
		tb.Errorf("Unexpected delta: got %v - %v = %g, want %g", a, b, delta, want)
	}
}

func TestBasicUNIXSockets(t *testing.T) {
	t.Skip("broken because unixSocket is in /var/run")
	unixSocket := "/var/run/mtail_test.socket"

	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "tail=2,filestream=2")()
	}
	logDir, rmLogDir := testutil.TestTempDir(t)
	defer rmLogDir()

	_, stopM := startUNIXSocketServer(t, 0, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
	defer stopM()

	startLineCount := getMetricFromUNIXSocket(t, unixSocket, "lines_total")
	startLogCount := getMetricFromUNIXSocket(t, unixSocket, "log_count")

	time.Sleep(1 * time.Second)

	logFile := filepath.Join(logDir, "log")

	f := testutil.TestOpenFile(t, logFile)

	for i := 1; i <= 3; i++ {
		testutil.WriteString(t, f, fmt.Sprintf("%d\n", i))
		time.Sleep(1 * time.Second)
	}

	endLineCount := getMetricFromUNIXSocket(t, unixSocket, "lines_total")
	endLogCount := getMetricFromUNIXSocket(t, unixSocket, "log_count")

	expectMetricDelta(t, endLineCount, startLineCount, 3)
	expectMetricDelta(t, endLogCount, startLogCount, 1)
}
