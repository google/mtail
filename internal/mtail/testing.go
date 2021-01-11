// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"context"
	"expvar"
	"fmt"
	"os"
	"runtime"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

const timeoutMultiplier = 3

const defaultDoOrTimeoutDeadline = 10 * time.Second

type TestServer struct {
	*Server

	w *watcher.LogWatcher

	tb testing.TB

	cancel context.CancelFunc

	// Set this to change the poll deadline when using DoOrTimeout within this TestServer.
	DoOrTimeoutDeadline time.Duration
}

// TestMakeServer makes a new TestServer for use in tests, but does not start
// the server.  If an error occurs during creation, a testing.Fatal is issued.
func TestMakeServer(tb testing.TB, pollInterval time.Duration, options ...Option) *TestServer {
	tb.Helper()

	// Reset counters when running multiple tests.  Tests that use expvar
	// helpers cannot be made parallel.
	glog.Info("resetting counters")
	expvar.Get("lines_total").(*expvar.Int).Set(0)
	expvar.Get("log_count").(*expvar.Int).Set(0)
	expvar.Get("log_rotations_total").(*expvar.Map).Init()
	expvar.Get("prog_loads_total").(*expvar.Map).Init()

	ctx, cancel := context.WithCancel(context.Background())
	w, err := watcher.NewLogWatcher(ctx, pollInterval)
	testutil.FatalIfErr(tb, err)
	m, err := New(ctx, metrics.NewStore(), w, options...)
	testutil.FatalIfErr(tb, err)
	return &TestServer{Server: m, w: w, tb: tb, cancel: cancel}
}

// TestStartServer creates a new TestServer and starts it running.  It
// returns the server, and a cleanup function.
func TestStartServer(tb testing.TB, pollInterval time.Duration, options ...Option) (*TestServer, func()) {
	tb.Helper()
	ts := TestMakeServer(tb, pollInterval, options...)
	return ts, ts.Start()
}

// Start starts the TestServer and returns a cleanup function.
func (ts *TestServer) Start() func() {
	ts.tb.Helper()
	errc := make(chan error, 1)
	go func() {
		err := ts.Run()
		errc <- err
	}()

	return func() {
		ts.cancel()

		select {
		case err := <-errc:
			testutil.FatalIfErr(ts.tb, err)
		case <-time.After(6 * time.Second):
			buf := make([]byte, 1<<16)
			n := runtime.Stack(buf, true)
			fmt.Fprintf(os.Stderr, "%s", buf[0:n])
			ts.tb.Fatal("timeout waiting for shutdown")
		}
	}
}

// Poll all watched logs for updates.
func (ts *TestServer) PollWatched() {
	glog.Info("TestServer polling watched objects")
	ts.w.Poll()
	ts.t.Poll()
	ts.l.LoadAllPrograms()
}

// TestGetExpvar fetches the expvar metric `name`, and returns the expvar.
// Callers are responsible for type assertions on the returned value.
func TestGetExpvar(tb testing.TB, name string) expvar.Var {
	tb.Helper()
	v := expvar.Get(name)
	glog.Infof("Var %q is %v", name, v)
	return v
}

/// GetExpvar is a helper function on TestServer that acts like TestGetExpvar.
func (ts *TestServer) GetExpvar(name string) expvar.Var {
	ts.tb.Helper()
	return TestGetExpvar(ts.tb, name)
}

// ExpectExpvarDeltaWithDeadline returns a deferrable function which tests if the expvar metric with name has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func (ts *TestServer) ExpectExpvarDeltaWithDeadline(name string, want int64) func() {
	ts.tb.Helper()
	deadline := ts.DoOrTimeoutDeadline
	if deadline == 0 {
		deadline = defaultDoOrTimeoutDeadline
	}
	start := TestGetExpvar(ts.tb, name).(*expvar.Int).Value()
	check := func() (bool, error) {
		ts.tb.Helper()
		now := TestGetExpvar(ts.tb, name).(*expvar.Int).Value()
		glog.Infof("now is %v", now)
		return now-start == want, nil
	}
	return func() {
		ts.tb.Helper()
		ok, err := testutil.DoOrTimeout(check, deadline, 10*time.Millisecond)
		testutil.FatalIfErr(ts.tb, err)
		if !ok {
			now := TestGetExpvar(ts.tb, name).(*expvar.Int).Value()
			ts.tb.Errorf("Did not see %s have delta by deadline: got %v - %v = %d, want %d", name, now, start, now-start, want)
		}
	}
}

// ExpectMapExpvarMetricDeltaWithDeadline returns a deferrable function which tests if the expvar map metric with name and key has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func (ts *TestServer) ExpectMapExpvarDeltaWithDeadline(name, key string, want int64) func() {
	ts.tb.Helper()
	deadline := ts.DoOrTimeoutDeadline
	if deadline == 0 {
		deadline = defaultDoOrTimeoutDeadline
	}
	startVar := TestGetExpvar(ts.tb, name).(*expvar.Map).Get(key)
	var start int64
	if startVar != nil {
		start = startVar.(*expvar.Int).Value()
	}
	check := func() (bool, error) {
		ts.tb.Helper()
		nowVar := TestGetExpvar(ts.tb, name).(*expvar.Map).Get(key)
		var now int64
		if nowVar != nil {
			now = nowVar.(*expvar.Int).Value()
		}
		return now-start == want, nil
	}
	return func() {
		ts.tb.Helper()
		ok, err := testutil.DoOrTimeout(check, deadline, 10*time.Millisecond)
		testutil.FatalIfErr(ts.tb, err)
		if !ok {
			nowVar := TestGetExpvar(ts.tb, name).(*expvar.Map).Get(key)
			var now int64
			if nowVar != nil {
				now = nowVar.(*expvar.Int).Value()
			}
			ts.tb.Errorf("Did not see %s[%s] have delta by deadline: got %v - %v = %d, want %d", name, key, now, start, now-start, want)
		}
	}
}

// GetProgramMetric fetches the datum of the program metric name.
func (ts *TestServer) GetProgramMetric(name string) datum.Datum {
	ts.tb.Helper()
	m := ts.store.Metrics[name]
	if len(m) != 1 || len(m[0].LabelValues) != 1 {
		ts.tb.Fatalf("Unexpected metric store content: expected a single metrics with no labels, but got %v", m)
		return nil
	}
	d, derr := m[0].GetDatum()
	testutil.FatalIfErr(ts.tb, derr)
	return d
}

// ExpectProgMetricDeltaWithDeadline tests that a given program metric increases by want within the deadline.  It assumes that the named metric is an Int type datum.Datum.
func (ts *TestServer) ExpectProgMetricDeltaWithDeadline(name string, want int64) func() {
	ts.tb.Helper()
	deadline := ts.DoOrTimeoutDeadline
	if deadline == 0 {
		deadline = defaultDoOrTimeoutDeadline
	}
	start := datum.GetInt(ts.GetProgramMetric(name))
	check := func() (bool, error) {
		ts.tb.Helper()
		now := datum.GetInt(ts.GetProgramMetric(name))
		return now-start == want, nil
	}
	return func() {
		ts.tb.Helper()
		ok, err := testutil.DoOrTimeout(check, deadline, 10*time.Millisecond)
		if err != nil {
			ts.tb.Fatal(err)
		}
		if !ok {
			now := datum.GetInt(ts.GetProgramMetric(name))
			delta := now - start
			ts.tb.Errorf("Did not see %s have delta by deadline: got %v - %v = %d, want %d", name, now, start, delta, want)
		}
	}
}
