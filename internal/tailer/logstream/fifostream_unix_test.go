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
	"time"

	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/tailer/logstream"
	"github.com/jaqx0r/mtail/internal/testutil"
	"github.com/jaqx0r/mtail/internal/waker"
	"golang.org/x/sys/unix"
)

func TestFifoStreamReadCompletedBecauseClosed(t *testing.T) {
	testutil.TimeoutTest(1*time.Second, func(t *testing.T) { //nolint:thelper
		var wg sync.WaitGroup

		tmpDir := testutil.TestTempDir(t)

		name := filepath.Join(tmpDir, "fifo")
		testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))

		ctx, cancel := context.WithCancel(context.Background())
		waker := waker.NewTestAlways()

		// In this and the following test, open RDWR so as to not block this thread
		// from proceeding.  If we open the logstream first, there is a race before
		// the write end opens that can sometimes lead to the logstream reading an
		// EOF (because the write end is not yet open) and the test fails.
		f, err := os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
		testutil.FatalIfErr(t, err)

		ps, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
		testutil.FatalIfErr(t, err)

		expected := []*logline.LogLine{
			{Context: context.TODO(), Filename: name, Line: "1"},
		}
		checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, ps.Lines())

		testutil.WriteString(t, f, "1\n")

		// Pipes need to be closed to signal to the pipeStream to finish up.
		testutil.FatalIfErr(t, f.Close())

		cancel() // no-op for pipes
		wg.Wait()

		checkLineDiff()

		if v := <-ps.Lines(); v != nil {
			t.Errorf("expecting pipestream to be complete because fifo closed")
		}
	})(t)
}

func TestFifoStreamReadCompletedBecauseCancel(t *testing.T) {
	testutil.TimeoutTest(1*time.Second, func(t *testing.T) { // nolint:thelper
		var wg sync.WaitGroup

		tmpDir := testutil.TestTempDir(t)

		name := filepath.Join(tmpDir, "fifo")
		testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))

		ctx, cancel := context.WithCancel(context.Background())
		waker := waker.NewTestAlways()

		f, err := os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
		testutil.FatalIfErr(t, err)

		ps, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
		testutil.FatalIfErr(t, err)
		expected := []*logline.LogLine{
			{Context: context.TODO(), Filename: name, Line: "1"},
		}
		checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, ps.Lines())

		testutil.WriteString(t, f, "1\n")

		cancel() // Cancellation here should cause the stream to shut down.
		wg.Wait()

		checkLineDiff()

		if v := <-ps.Lines(); v != nil {
			t.Errorf("expecting pipestream to be complete because cancelled")
		}
	})(t)
}

func TestFifoStreamReadURL(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "fifo")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))

	ctx, cancel := context.WithCancel(context.Background())
	// The stream is not shut down by cancel in this test.
	defer cancel()
	waker := waker.NewTestAlways()

	ps, err := logstream.New(ctx, &wg, waker, "file://"+name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "1"},
		{Context: context.TODO(), Filename: name, Line: "2"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, ps.Lines())

	f, err := os.OpenFile(name, os.O_WRONLY, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)
	testutil.WriteString(t, f, "1\n")

	// Give the stream a chance to wake and read
	time.Sleep(10 * time.Millisecond)

	testutil.WriteString(t, f, "2\n")

	// Pipes need to be closed to signal to the pipeStream to finish up.
	testutil.FatalIfErr(t, f.Close())

	wg.Wait()

	checkLineDiff()

	if v := <-ps.Lines(); v != nil {
		t.Errorf("expecting pipestream to be complete because fifo closed")
	}

	cancel() // no-op for pipes
}

func TestFifoStreamReadStdin(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "fakestdin")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))
	f, err := os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)
	testutil.OverrideStdin(t, f)
	testutil.WriteString(t, f, "1\n")

	ctx, cancel := context.WithCancel(context.Background())
	// The stream is not shut down by cancel in this test.
	defer cancel()
	waker := waker.NewTestAlways()

	ps, err := logstream.New(ctx, &wg, waker, "-", logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: "-", Line: "1"},
		{Context: context.TODO(), Filename: "-", Line: "2"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, ps.Lines())

	testutil.WriteString(t, f, "2\n")

	// Give the stream a chance to wake and read
	time.Sleep(10 * time.Millisecond)

	testutil.FatalIfErr(t, f.Close())

	wg.Wait()

	checkLineDiff()

	if v := <-ps.Lines(); v != nil {
		t.Errorf("expecting pipestream to be complete beacuse fifo closed")
	}
}
