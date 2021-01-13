// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"expvar"
	"testing"
	"time"

	"github.com/golang/glog"
)

// TestGetExpvar fetches the expvar metric `name`, and returns the expvar.
// Callers are responsible for type assertions on the returned value.
func TestGetExpvar(tb testing.TB, name string) expvar.Var {
	tb.Helper()
	v := expvar.Get(name)
	glog.Infof("Var %q is %v", name, v)
	return v
}

const defaultDoOrTimeoutDeadline = 10 * time.Second

// ExpectExpvarDeltaWithDeadline returns a deferrable function which tests if the expvar metric with name has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func ExpectExpvarDeltaWithDeadline(tb testing.TB, name string, want int64) func() {
	tb.Helper()
	deadline := defaultDoOrTimeoutDeadline
	start := TestGetExpvar(tb, name).(*expvar.Int).Value()
	check := func() (bool, error) {
		tb.Helper()
		now := TestGetExpvar(tb, name).(*expvar.Int).Value()
		glog.Infof("now is %v", now)
		return now-start == want, nil
	}
	return func() {
		tb.Helper()
		ok, err := DoOrTimeout(check, deadline, 10*time.Millisecond)
		FatalIfErr(tb, err)
		if !ok {
			now := TestGetExpvar(tb, name).(*expvar.Int).Value()
			tb.Errorf("Did not see %s have delta by deadline: got %v - %v = %d, want %d", name, now, start, now-start, want)
		}
	}
}

// ExpectMapExpvarMetricDeltaWithDeadline returns a deferrable function which tests if the expvar map metric with name and key has changed by delta within the given deadline, once the function begins.  Before returning, it fetches the original value for comparison.
func ExpectMapExpvarDeltaWithDeadline(tb testing.TB, name, key string, want int64) func() {
	tb.Helper()
	deadline := defaultDoOrTimeoutDeadline
	startVar := TestGetExpvar(tb, name).(*expvar.Map).Get(key)
	var start int64
	if startVar != nil {
		start = startVar.(*expvar.Int).Value()
	}
	check := func() (bool, error) {
		tb.Helper()
		nowVar := TestGetExpvar(tb, name).(*expvar.Map).Get(key)
		var now int64
		if nowVar != nil {
			now = nowVar.(*expvar.Int).Value()
		}
		return now-start == want, nil
	}
	return func() {
		tb.Helper()
		ok, err := DoOrTimeout(check, deadline, 10*time.Millisecond)
		FatalIfErr(tb, err)
		if !ok {
			nowVar := TestGetExpvar(tb, name).(*expvar.Map).Get(key)
			var now int64
			if nowVar != nil {
				now = nowVar.(*expvar.Int).Value()
			}
			tb.Errorf("Did not see %s[%s] have delta by deadline: got %v - %v = %d, want %d", name, key, now, start, now-start, want)
		}
	}
}
