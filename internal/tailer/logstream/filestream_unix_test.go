// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix

package logstream_test

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

// TestFileStreamRotation is a unix-specific test because on Windows, files cannot be removed
// or renamed while there is an open read handle on them. Instead, log rotation would
// have to be implemented by copying and then truncating the original file. That test
// case is already covered by TestFileStreamTruncation.
func TestFileStreamRotation(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")

	// OneShotDisabled because we hit EOF and need to wait.
	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "1"},
		{Context: context.TODO(), Filename: name, Line: "2"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	awaken(1, 1) // sync to eof

	glog.Info("write 1")
	testutil.WriteString(t, f, "1\n")
	awaken(1, 1)

	glog.Info("rename")
	testutil.FatalIfErr(t, os.Rename(name, name+".1"))
	// filestream won't notice if there's a synchronisation point between
	// rename and create, that path relies on the tailer
	f = testutil.TestOpenFile(t, name)
	defer f.Close()

	awaken(1, 1)
	glog.Info("write 2")
	testutil.WriteString(t, f, "2\n")
	awaken(1, 1)

	cancel()
	wg.Wait()

	checkLineDiff()

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stopped")
	}
}

func TestFileStreamURL(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := logstream.New(ctx, &wg, waker, "file://"+name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "yo"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	awaken(1, 1)

	testutil.WriteString(t, f, "yo\n")
	awaken(1, 1)

	cancel()
	wg.Wait()

	checkLineDiff()

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stopped")
	}
}

// TestFileStreamOpenFailure is a unix-specific test because on Windows, it is not possible to create a file
// that you yourself cannot read (minimum permissions are 0222).
func TestFileStreamOpenFailure(t *testing.T) {
	// can't force a permission denied if run as root
	testutil.SkipIfRoot(t)
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f, err := os.OpenFile(name, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0)
	//nolint:staticcheck // test code
	defer f.Close()

	testutil.FatalIfErr(t, err)

	ctx, cancel := context.WithCancel(context.Background())
	waker, _ := waker.NewTest(ctx, 0, "stream")

	_, err = logstream.New(ctx, &wg, waker, name, logstream.OneShotEnabled)
	if err == nil || !os.IsPermission(err) {
		t.Errorf("Expected a permission denied error, got: %v", err)
	}
	cancel()
}
