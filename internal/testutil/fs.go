// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"io/ioutil"
	"os"
	"testing"
)

// TestTempDir creates a temporary directory for use during tests, returning the pathname.
func TestTempDir(tb testing.TB) string {
	tb.Helper()
	name, err := ioutil.TempDir("", "mtail-test")
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
	f, err := os.OpenFile(name, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0600)
	if err != nil {
		tb.Fatal(err)
	}
	return f
}

// OpenLogFile creates a new file that emulates being a log.
func OpenLogFile(tb testing.TB, name string) *os.File {
	tb.Helper()
	f, err := os.OpenFile(name, os.O_CREATE|os.O_TRUNC|os.O_WRONLY|os.O_APPEND, 0600)
	if err != nil {
		tb.Fatal(err)
	}
	return f
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
