// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"runtime"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/watcher"
)

const timeoutMultiplier = 3

// TestTempDir creates a temporary directory for use during tests. It returns
// the pathname, and a cleanup function.
func TestTempDir(tb testing.TB) (string, func()) {
	tb.Helper()
	name, err := ioutil.TempDir("", "mtail-test")
	if err != nil {
		tb.Fatal(err)
	}
	return name, func() {
		if err := os.RemoveAll(name); err != nil {
			tb.Fatalf("os.RemoveAll(%s): %s", name, err)
		}
	}
}

// TestMakeServer makes a new Server for use in tests, but does not start
// the server.  It returns the server, or any errors the new server creates.
func TestMakeServer(tb testing.TB, pollInterval time.Duration, disableFsNotify bool, options ...func(*Server) error) (*Server, error) {
	tb.Helper()
	w, err := watcher.NewLogWatcher(pollInterval, !disableFsNotify)
	if err != nil {
		tb.Fatal(err)
	}

	return New(metrics.NewStore(), w, options...)
}

// TestStartServer creates a new Server and starts it running.  It
// returns the server, and a cleanup function.
func TestStartServer(tb testing.TB, pollInterval time.Duration, disableFsNotify bool, options ...func(*Server) error) (*Server, func()) {
	tb.Helper()
	options = append(options, BindAddress("", "0"))

	m, err := TestMakeServer(tb, pollInterval, disableFsNotify, options...)
	if err != nil {
		tb.Fatal(err)
	}

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
		tb.Fatal("server wasn't listening after 10 attempts")
	}

	return m, func() {
		err := m.Close()
		if err != nil {
			tb.Fatal(err)
		}
		select {
		case err = <-errc:
		case <-time.After(5 * time.Second):
			buf := make([]byte, 1<<16)
			n := runtime.Stack(buf, true)
			fmt.Fprintf(os.Stderr, "%s", buf[0:n])
			tb.Fatal("timeout waiting for shutdown")
		}

		if err != nil {
			tb.Fatal(err)
		}
	}
}

// TestGetMetric fetches the expvar metrics from the Server at addr, and
// returns the value of one named name.  Callers are responsible for type
// assertions on the returned value.
func TestGetMetric(tb testing.TB, addr, name string) interface{} {
	uri := fmt.Sprintf("http://%s/debug/vars", addr)
	client := &http.Client{
		Timeout: time.Duration(5 * time.Second),
	}
	resp, err := client.Get(uri)
	if err != nil {
		tb.Fatal(err)
	}
	buf := new(bytes.Buffer)
	buf.ReadFrom(resp.Body)
	var r map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &r); err != nil {
		tb.Fatalf("%s: body was %s", err, buf.String())
	}
	return r[name]
}

// TestChdir changes current working directory, and returns a cleanup function
// to return to the previous directory.
func TestChdir(tb testing.TB, dir string) func() {
	tb.Helper()
	cwd, err := os.Getwd()
	if err != nil {
		tb.Fatal(err)
	}
	err = os.Chdir(dir)
	if err != nil {
		tb.Fatal(err)
	}
	return func() {
		err := os.Chdir(cwd)
		if err != nil {
			tb.Fatal(err)
		}
	}
}

// ExpectMetricDelta checks to see if the difference between a and b is want;
// it assumes both values are float64s that came from a TestGetMetric.
func ExpectMetricDelta(tb testing.TB, a, b interface{}, want float64) {
	tb.Helper()
	if a == nil {
		a = 0.
	}
	if b == nil {
		b = 0.
	}
	if a.(float64)-b.(float64) != want {
		tb.Errorf("Unexpected delta: got %v - %v, want %g", a, b, want)
	}
}

// TestOpenFile creates a new file called name and returns the opened file.
func TestOpenFile(tb testing.TB, name string) *os.File {
	tb.Helper()
	f, err := os.OpenFile(name, os.O_CREATE|os.O_RDWR|os.O_APPEND, 0600)
	if err != nil {
		tb.Fatal(err)
	}
	return f
}

// TestSetFlag sets the value of the commandline flag, and returns a cleanup function that restores the flag value.
func TestSetFlag(tb testing.TB, name, value string) func() {
	tb.Helper()
	val := flag.Lookup(name)

	flag.Set(name, value)
	flag.Parse()

	return func() {
		if val != nil {
			flag.Set(name, val.Value.String())
		}
	}
}
