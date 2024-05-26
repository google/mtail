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
	"github.com/google/mtail/internal/waker"
)

const defaultDoOrTimeoutDeadline = 10 * time.Second

type TestServer struct {
	*Server

	streamWaker waker.Waker // for idle logstreams; others are polled explicitly in PollWatched
	// AwakenLogStreams wakes n log streams.  This acts as a barrier method,
	// synchronising the logstreams and the test.
	AwakenLogStreams waker.WakeFunc

	patternWaker waker.Waker // polling for new glob pattern matches
	// AwakenPatternPollers wakes n pattern pollers.  This acts as a barrier
	// method, synchronising the pattern poll with the test.
	AwakenPatternPollers waker.WakeFunc // the glob awakens

	gcWaker        waker.Waker // activate the cleanup routines
	AwakenGcPoller waker.WakeFunc

	tb testing.TB

	cancel context.CancelFunc

	// Set this to change the poll deadline when using DoOrTimeout within this TestServer.
	DoOrTimeoutDeadline time.Duration
}

// TestMakeServer makes a new TestServer for use in tests, but does not start
// the server.  If an error occurs during creation, a testing.Fatal is issued.
func TestMakeServer(tb testing.TB, patternWakers int, streamWakers int, options ...Option) *TestServer {
	tb.Helper()

	// Reset counters when running multiple tests.  Tests that use expvar
	// helpers cannot be made parallel.
	glog.Info("resetting counters")
	expvar.Get("lines_total").(*expvar.Int).Set(0)
	expvar.Get("log_count").(*expvar.Int).Set(0)
	expvar.Get("log_lines_total").(*expvar.Map).Init()
	expvar.Get("log_opens_total").(*expvar.Map).Init()
	expvar.Get("log_closes_total").(*expvar.Map).Init()
	expvar.Get("file_truncates_total").(*expvar.Map).Init()
	expvar.Get("prog_loads_total").(*expvar.Map).Init()

	ctx, cancel := context.WithCancel(context.Background())
	ts := &TestServer{
		tb:     tb,
		cancel: cancel,
	}
	ts.streamWaker, ts.AwakenLogStreams = waker.NewTest(ctx, streamWakers, "streams")
	ts.patternWaker, ts.AwakenPatternPollers = waker.NewTest(ctx, patternWakers, "patterns")
	ts.gcWaker, ts.AwakenGcPoller = waker.NewTest(ctx, 1, "gc")
	options = append(options,
		LogstreamPollWaker(ts.streamWaker),
		LogPatternPollWaker(ts.patternWaker),
		GcWaker(ts.gcWaker),
	)
	var err error
	ts.Server, err = New(ctx, metrics.NewStore(), options...)
	testutil.FatalIfErr(tb, err)
	return ts
}

// TestStartServer creates a new TestServer and starts it running.  It returns
// the server, and a stop function.  `patternWakers` indicates the number of
// expected pattern wakers to wait for at this moment; usually 1 because the
// test server is started with a `LogPathPattern`.  `streamWakers` indiecates
// the number of expected stream wakers to wait for at this moment.  The value
// of this parameter shuld be the number of log files created in test
// (e.g. with `testutil.TestOpenFile`) before invoking this function.
func TestStartServer(tb testing.TB, patternWakers int, streamWakers int, options ...Option) (*TestServer, func()) {
	tb.Helper()
	ts := TestMakeServer(tb, patternWakers, streamWakers, options...)
	return ts, ts.Start()
}

// Start starts the TestServer and returns a stop function.
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

func (ts *TestServer) LoadAllPrograms() {
	ts.tb.Helper()
	glog.Infof("TestServer reloading programs")
	if err := ts.r.LoadAllPrograms(); err != nil {
		glog.Info(err)
		ts.tb.Log(err)
	}
}

// GetExpvar is a helper function on TestServer that acts like TestGetExpvar.
func (ts *TestServer) GetExpvar(name string) expvar.Var {
	ts.tb.Helper()
	return testutil.TestGetExpvar(ts.tb, name)
}

// ExpectExpvarDeltaWithDeadline returns a deferrable function which tests if the expvar metric with name has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func (ts *TestServer) ExpectExpvarDeltaWithDeadline(name string, want int64) func() {
	ts.tb.Helper()
	return testutil.ExpectExpvarDeltaWithDeadline(ts.tb, name, want)
}

// ExpectMapExpvarMetricDeltaWithDeadline returns a deferrable function which tests if the expvar map metric with name and key has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func (ts *TestServer) ExpectMapExpvarDeltaWithDeadline(name, key string, want int64) func() {
	ts.tb.Helper()
	return testutil.ExpectMapExpvarDeltaWithDeadline(ts.tb, name, key, want)
}

// GetProgramMetric fetches the datum of the program metric name.
func (ts *TestServer) GetProgramMetric(name, prog string) datum.Datum {
	ts.tb.Helper()
	m := ts.store.FindMetricOrNil(name, prog)
	if m == nil {
		ts.tb.Fatalf("Unexpected metric store content, got nil instead of %s %s", name, prog)
		return nil
	}
	d, derr := m.GetDatum()
	testutil.FatalIfErr(ts.tb, derr)
	return d
}

// ExpectProgMetricDeltaWithDeadline tests that a given program metric increases by want within the deadline.  It assumes that the named metric is an Int type datum.Datum.
func (ts *TestServer) ExpectProgMetricDeltaWithDeadline(name, prog string, want int64) func() {
	ts.tb.Helper()
	deadline := ts.DoOrTimeoutDeadline
	if deadline == 0 {
		deadline = defaultDoOrTimeoutDeadline
	}
	start := datum.GetInt(ts.GetProgramMetric(name, prog))
	check := func() (bool, error) {
		ts.tb.Helper()
		now := datum.GetInt(ts.GetProgramMetric(name, prog))
		return now-start == want, nil
	}
	return func() {
		ts.tb.Helper()
		ok, err := testutil.DoOrTimeout(check, deadline, 10*time.Millisecond)
		if err != nil {
			ts.tb.Fatal(err)
		}
		if !ok {
			now := datum.GetInt(ts.GetProgramMetric(name, prog))
			delta := now - start
			ts.tb.Errorf("Did not see %s have delta by deadline: got %v - %v = %d, want %d", name, now, start, delta, want)
		}
	}
}
