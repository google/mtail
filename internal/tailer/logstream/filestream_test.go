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
	"github.com/google/mtail/internal/tailer/waker"
	"github.com/google/mtail/internal/testutil"
)

func TestFileStreamRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	ps := NewStubProcessor()
	waker, awaken := waker.NewTest(1) // Just one waker to wait on.

	ctx, cancel := context.WithCancel(context.Background())
	fs, err := logstream.New(ctx, &wg, waker, name, ps)
	testutil.FatalIfErr(t, err)
	awaken()

	ps.ExpectLinesReceived(1)
	testutil.WriteString(t, f, "yo\n")
	awaken()

	ps.Verify()
	expected := []logline.LogLine{
		{context.TODO(), name, "yo"},
	}
	testutil.ExpectNoDiff(t, expected, ps.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))
	cancel()
	wg.Wait()

	if !fs.IsFinished() {
		t.Errorf("expecting filestream to be closed because cancellation")
	}
}

func TestFileStreamRotation(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	ps := NewStubProcessor()
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())

	_, err := logstream.New(ctx, &wg, waker, name, ps)
	testutil.FatalIfErr(t, err)
	awaken()

	ps.ExpectLinesReceived(2)

	glog.Info("write 1")
	testutil.WriteString(t, f, "1\n")
	awaken()

	testutil.FatalIfErr(t, os.Rename(name, name+".1"))

	f = testutil.TestOpenFile(t, name)
	glog.Info("write 2")
	testutil.WriteString(t, f, "2\n")

	ps.Verify()

	expected := []logline.LogLine{
		{context.TODO(), name, "1"},
		{context.TODO(), name, "2"},
	}
	testutil.ExpectNoDiff(t, expected, ps.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()

	wg.Wait()
}

func TestFileStreamTruncation(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "log")
	f := testutil.OpenLogFile(t, name)
	ps := NewStubProcessor()
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())
	_, err := logstream.New(ctx, &wg, waker, name, ps)
	testutil.FatalIfErr(t, err)
	awaken() // Synchronise past first read after seekToEnd

	ps.ExpectLinesReceived(3)

	testutil.WriteString(t, f, "1\n2\n")
	awaken()
	testutil.FatalIfErr(t, f.Close())
	awaken()
	f = testutil.OpenLogFile(t, name)
	testutil.WriteString(t, f, "3\n")
	awaken()

	ps.Verify()

	expected := []logline.LogLine{
		{context.TODO(), name, "1"},
		{context.TODO(), name, "2"},
		{context.TODO(), name, "3"},
	}
	testutil.ExpectNoDiff(t, expected, ps.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()

	wg.Wait()
}

func TestFileStreamFinishedBecauseRemoved(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	ps := NewStubProcessor()
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())
	fs, err := logstream.New(ctx, &wg, waker, name, ps)
	testutil.FatalIfErr(t, err)
	awaken() // Synchronise past first read after seekToEnd

	ps.ExpectLinesReceived(1)
	testutil.WriteString(t, f, "yo\n")
	go awaken()

	testutil.FatalIfErr(t, f.Close())
	testutil.FatalIfErr(t, os.Remove(name))
	//awaken() //-- deadlock as IsFinished() , TODO(jaq) nonblocking wake

	ps.Verify()
	expected := []logline.LogLine{
		{context.TODO(), name, "yo"},
	}
	testutil.ExpectNoDiff(t, expected, ps.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	wg.Wait() // don't cancel first, so that we exit from the file not found

	if !fs.IsFinished() {
		t.Errorf("expecting filestream to be closed because log was removed")
	}
	cancel()
}
