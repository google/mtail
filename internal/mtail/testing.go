// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"bytes"
	"encoding/json"
	"expvar"
	"fmt"
	"net"
	"net/http"
	"os"
	"runtime"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

const timeoutMultiplier = 3

type TestServer struct {
	*Server

	w *watcher.LogWatcher

	tb testing.TB

	// Set this to change the poll deadline when using DoOrTimeout within this TestServer.
	DoOrTimeoutDeadline time.Duration
}

// TestMakeServer makes a new TestServer for use in tests, but does not start
// the server.  If an error occurs during creation, a testing.Fatal is issued.
func TestMakeServer(tb testing.TB, pollInterval time.Duration, enableFsNotify bool, options ...func(*Server) error) *TestServer {
	tb.Helper()

	expvar.Get("lines_total").(*expvar.Int).Set(0)
	expvar.Get("log_count").(*expvar.Int).Set(0)
	expvar.Get("log_rotations_total").(*expvar.Map).Init()
	expvar.Get("prog_loads_total").(*expvar.Map).Init()

	w, err := watcher.NewLogWatcher(pollInterval, enableFsNotify)
	testutil.FatalIfErr(tb, err)
	m, err := New(metrics.NewStore(), w, options...)
	if err != nil {
		tb.Fatal(err)
	}
	return &TestServer{Server: m, w: w, tb: tb}
}

// TestStartServer creates a new TestServer and starts it running.  It
// returns the server, and a cleanup function.
func TestStartServer(tb testing.TB, pollInterval time.Duration, enableFsNotify bool, options ...func(*Server) error) (*TestServer, func()) {
	tb.Helper()
	options = append(options, BindAddress("", "0"))

	m := TestMakeServer(tb, pollInterval, enableFsNotify, options...)
	return m, m.Start()
}

// Start starts the TestServer and returns a cleanup function.
func (m *TestServer) Start() func() {
	m.tb.Helper()
	errc := make(chan error, 1)
	go func() {
		err := m.Run()
		errc <- err
	}()

	glog.Infof("check that server is listening")
	count := 0
	for _, err := net.DialTimeout("tcp", m.Addr(), 10*time.Millisecond*timeoutMultiplier); err != nil && count < 10; count++ {
		glog.Infof("err: %s, retrying to dial %s", err, m.Addr())
		time.Sleep(100 * time.Millisecond * timeoutMultiplier)
	}
	if count >= 10 {
		m.tb.Fatal("server wasn't listening after 10 attempts")
	}

	return func() {
		err := m.Close(true)
		if err != nil {
			m.tb.Fatal(err)
		}

		select {
		case err = <-errc:
		case <-time.After(5 * time.Second):
			buf := make([]byte, 1<<16)
			n := runtime.Stack(buf, true)
			fmt.Fprintf(os.Stderr, "%s", buf[0:n])
			m.tb.Fatal("timeout waiting for shutdown")
		}
		if err != nil {
			m.tb.Fatal(err)
		}
	}
}

// Poll all watched logs for updates.
func (m *TestServer) PollWatched() {
	glog.Infof("TestServer polling watched objects")
	m.w.Poll()
}

// TestGetMetric fetches the expvar metrics from the Server at addr, and
// returns the value of one named name.  Callers are responsible for type
// assertions on the returned value.
func TestGetMetric(tb testing.TB, addr, name string) interface{} {
	tb.Helper()
	uri := fmt.Sprintf("http://%s/debug/vars", addr)
	client := &http.Client{
		Timeout: 5 * time.Second,
	}
	resp, err := client.Get(uri)
	if err != nil {
		tb.Fatal(err)
	}
	buf := new(bytes.Buffer)
	n, err := buf.ReadFrom(resp.Body)
	resp.Body.Close()
	testutil.FatalIfErr(tb, err)
	glog.V(2).Infof("TestGetMetric: http client read %d bytes from debug/vars", n)
	var r map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &r); err != nil {
		tb.Fatalf("%s: body was %s", err, buf.String())
	}
	glog.V(2).Infof("TestGetMetric: returned value for %s: %v", name, r[name])
	return r[name]
}

// TestMetricDelta checks to see if the difference between a and b is want;
// it assumes both values are float64s that came from a TestGetMetric.
func TestMetricDelta(a, b interface{}) float64 {
	if a == nil {
		a = 0.
	}
	if b == nil {
		b = 0.
	}
	return a.(float64) - b.(float64)
}

// ExpectMetricDelta checks to see if the difference between a and b is want;
// it assumes both values are float64s that came from a TestGetMetric.
func ExpectMetricDelta(tb testing.TB, a, b interface{}, want float64) {
	tb.Helper()
	delta := TestMetricDelta(a, b)
	if delta != want {
		tb.Errorf("Unexpected delta: got %v - %v = %g, want %g", a, b, delta, want)
	}
}

// ExpectMetricDeltaWithDeadline returns a deferrable function which tests if the metric with name has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func (ts *TestServer) ExpectMetricDeltaWithDeadline(name string, want float64) func() {
	ts.tb.Helper()
	deadline := ts.DoOrTimeoutDeadline
	if deadline == 0 {
		deadline = time.Minute
	}
	start := TestGetMetric(ts.tb, ts.Addr(), name)
	check := func() (bool, error) {
		ts.tb.Helper()
		now := TestGetMetric(ts.tb, ts.Addr(), name)
		return TestMetricDelta(now, start) == want, nil
	}
	return func() {
		ts.tb.Helper()
		ok, err := testutil.DoOrTimeout(check, deadline, 10*time.Millisecond)
		if err != nil {
			ts.tb.Fatal(err)
		}
		if !ok {
			now := TestGetMetric(ts.tb, ts.Addr(), name)
			delta := TestMetricDelta(now, start)
			ts.tb.Errorf("Did not see delta by deadline: got %v - %v = %g, want %g", now, start, delta, want)
		}
	}
}

// ExpectMapMetricDeltaWithDeadline returns a deferrable function which tests if the map metric with name and key has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func (ts *TestServer) ExpectMapMetricDeltaWithDeadline(name, key string, want float64) func() {
	ts.tb.Helper()
	deadline := ts.DoOrTimeoutDeadline
	if deadline == 0 {
		deadline = time.Minute
	}
	start := TestGetMetric(ts.tb, ts.Addr(), name).(map[string]interface{})
	check := func() (bool, error) {
		ts.tb.Helper()
		now := TestGetMetric(ts.tb, ts.Addr(), name).(map[string]interface{})
		return TestMetricDelta(now[key], start[key]) == want, nil
	}
	return func() {
		ts.tb.Helper()
		ok, err := testutil.DoOrTimeout(check, deadline, 10*time.Millisecond)
		if err != nil {
			ts.tb.Fatal(err)
		}
		if !ok {
			now := TestGetMetric(ts.tb, ts.Addr(), name).(map[string]interface{})
			delta := TestMetricDelta(now[key], start[key])
			ts.tb.Errorf("Did not see delta by deadline: got %v - %v = %g, want %g", now[key], start[key], delta, want)
		}
	}
}

// logWatcherTestTable contains reusable inputs to NewLogWatcher under test.
var LogWatcherTestTable = []struct {
	PollInterval   time.Duration
	EnableFsNotify bool
}{
	{0, true},                      // notify only
	{10 * time.Millisecond, false}, // poll only
	//{10 * time.Millisecond, true},  // both --- TODO: breaks permission test,
}
