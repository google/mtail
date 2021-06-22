// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
package testutil

import (
	"os"
	"runtime"
	"testing"
	"time"

	"github.com/golang/glog"
)

// DoOrTimeout runs a check function every interval until deadline, unless the
// check returns true.  The check should return false otherwise. If the check
// returns an error the check is immediately failed.
func DoOrTimeout(do func() (bool, error), deadline, interval time.Duration) (bool, error) {
	timeout := time.After(deadline)
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	for {
		select {
		case <-timeout:
			return false, nil
		case <-ticker.C:
			glog.V(2).Infof("tick")
			ok, err := do()
			glog.V(2).Infof("ok, err: %v %v", ok, err)
			if err != nil {
				return false, err
			}
			if ok {
				return true, nil
			}
			// otherwise wait and retry
		}
	}
}

// TimeoutTest returns a test function that executes f with a timeout, If the
// test does not complete in time the test is failed.  This lets us set a
// per-test timeout instead of the global `go test -timeout` coarse timeout.
func TimeoutTest(timeout time.Duration, f func(t *testing.T)) func(t *testing.T) {
	// Raise the timeout if we're run under the race detector.
	timeout = timeout * RaceDetectorMultiplier
	// If we're in a CI environment, raise the timeout by 10x.  This mimics the
	// timeout gloabl flag set in the Makefile.
	if os.Getenv("CI") == "true" {
		timeout = 10 * timeout
	}
	return func(t *testing.T) {
		t.Helper()
		done := make(chan bool)
		go func() {
			t.Helper()
			defer close(done)
			f(t)
		}()
		select {
		case <-time.After(timeout):
			buf := make([]byte, 1<<20)
			stacklen := runtime.Stack(buf, true)
			t.Fatalf("timed out\n%s", buf[:stacklen])
		case <-done:
		}
	}
}
