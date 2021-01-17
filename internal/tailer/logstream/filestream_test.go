// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

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

func TestFileStreamRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)
	fs, err := logstream.New(ctx, &wg, waker, name, lines, true)
	testutil.FatalIfErr(t, err)
	awaken(1)

	testutil.WriteString(t, f, "yo\n")
	awaken(1)

	fs.Stop()
	wg.Wait()
	close(lines)
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "yo"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stopped")
	}
	cancel()
	wg.Wait()

}

func TestFileStreamRotation(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	lines := make(chan *logline.LogLine, 2)

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	fs, err := logstream.New(ctx, &wg, waker, name, lines, true)
	testutil.FatalIfErr(t, err)
	awaken(1)

	glog.Info("write 1")
	testutil.WriteString(t, f, "1\n")
	awaken(1)

	glog.Info("rename")
	testutil.FatalIfErr(t, os.Rename(name, name+".1"))
	// filestream won't notice if there's a synchronisation point between
	// rename and create, that path relies on the tailer
	f = testutil.TestOpenFile(t, name)
	awaken(1)
	glog.Info("write 2")
	testutil.WriteString(t, f, "2\n")
	awaken(1)

	fs.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
		{context.TODO(), name, "2"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()
}

func TestFileStreamTruncation(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.OpenLogFile(t, name)
	lines := make(chan *logline.LogLine, 3)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)
	fs, err := logstream.New(ctx, &wg, waker, name, lines, true)
	testutil.FatalIfErr(t, err)
	awaken(1) // Synchronise past first read after seekToEnd

	testutil.WriteString(t, f, "1\n2\n")
	awaken(1)
	testutil.FatalIfErr(t, f.Close())
	awaken(1)
	f = testutil.OpenLogFile(t, name)
	testutil.WriteString(t, f, "3\n")
	awaken(1)

	fs.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)

	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
		{context.TODO(), name, "2"},
		{context.TODO(), name, "3"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()
}

func TestFileStreamFinishedBecauseCancel(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	fs, err := logstream.New(ctx, &wg, waker, name, lines, true)
	testutil.FatalIfErr(t, err)
	awaken(1) // Synchronise past first read after seekToEnd

	testutil.WriteString(t, f, "yo\n")
	awaken(1)

	cancel()
	wg.Wait()
	close(lines) // Signal it's time to go.

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "yo"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stream was cancelled")
	}
}

func TestFileStreamPartialRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	fs, err := logstream.New(ctx, &wg, waker, name, lines, true)
	testutil.FatalIfErr(t, err)
	awaken(1)

	testutil.WriteString(t, f, "yo")
	awaken(1)

	// received := testutil.LinesReceived(lines)
	// expected := []*logline.LogLine{}
	// testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	testutil.WriteString(t, f, "\n")
	awaken(1)

	fs.Stop()
	wg.Wait()
	close(lines)
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "yo"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because cancellation")
	}

	cancel()
	wg.Wait()
}

func TestFileStreamOpenFailure(t *testing.T) {
	// can't force a permission denied if run as root
	testutil.SkipIfRoot(t)
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	_, err := os.OpenFile(name, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0)
	testutil.FatalIfErr(t, err)

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, _ := waker.NewTest(ctx, 0)

	_, err = logstream.New(ctx, &wg, waker, name, lines, true)
	if err == nil || !os.IsPermission(err) {
		t.Errorf("Expected a permission denied error, got: %v", err)
	}
	cancel()
}
