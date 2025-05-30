// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"os"
	"path/filepath"
	"testing"
)

// TestTempDir creates a temporary directory for use during tests, returning the pathname.
//
// We don't use tb.TempDir() beacuse generates temp directories based on the
// test name, which is often longer than 108 characters, which then breaks the
// unix socket tests because elsewhere in Go only 108 character socket
// pathnames are supported!.  https://github.com/golang/go/issues/6895
func TestTempDir(tb testing.TB) string {
	tb.Helper()
	//nolint:usetesting
	name, err := os.MkdirTemp("", "mtail-test")
	if err != nil {
		tb.Fatal(err)
	}
	tb.Cleanup(func() {
		if err := os.RemoveAll(name); err != nil {
			tb.Fatalf("os.RemoveAll(%s): %s", name, err)
		}
	})
	return name
}

// TestOpenFile creates a new file called name and returns the opened file.
func TestOpenFile(tb testing.TB, name string) *os.File {
	tb.Helper()
	f, err := os.OpenFile(filepath.Clean(name), os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600)
	if err != nil {
		tb.Fatal(err)
	}
	return f
}

// OpenLogFile creates a new file that emulates being a log.
func OpenLogFile(tb testing.TB, name string) *os.File {
	tb.Helper()
	f, err := os.OpenFile(filepath.Clean(name), os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o600)
	if err != nil {
		tb.Fatal(err)
	}
	return f
}

// Chdir changes current working directory, and registers a cleanup function
// to return to the previous directory.
func Chdir(tb testing.TB, dir string) {
	tb.Helper()
	cwd, err := os.Getwd()
	if err != nil {
		tb.Fatal(err)
	}
	err = os.Chdir(dir)
	if err != nil {
		tb.Fatal(err)
	}
	tb.Cleanup(func() {
		err := os.Chdir(cwd)
		if err != nil {
			tb.Fatal(err)
		}
	})
}
