// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix
// +build unix

package logstream_test

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
	"golang.org/x/sys/unix"
)

func TestPipeStreamReadCompletedBecauseClosed(t *testing.T) {
	testutil.TimeoutTest(1*time.Second, func(t *testing.T) { //nolint:thelper
		var wg sync.WaitGroup

		tmpDir := testutil.TestTempDir(t)

		name := filepath.Join(tmpDir, "fifo")
		testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))

		lines := make(chan *logline.LogLine, 1)
		ctx, cancel := context.WithCancel(context.Background())
		waker := waker.NewTestAlways()

		// In this and the following test, open RDWR so as to not block this thread
		// from proceeding.  If we open the logstream first, there is a race before
		// the write end opens that can sometimes lead to the logstream reading an
		// EOF (because the write end is not yet open) and the test fails.
		f, err := os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
		testutil.FatalIfErr(t, err)

		ps, err := logstream.New(ctx, &wg, waker, name, lines, false)
		testutil.FatalIfErr(t, err)

		testutil.WriteString(t, f, "1\n")

		// Pipes need to be closed to signal to the pipeStream to finish up.
		testutil.FatalIfErr(t, f.Close())

		ps.Stop() // no-op for pipes
		wg.Wait()
		close(lines)

		received := testutil.LinesReceived(lines)
		expected := []*logline.LogLine{
			{context.TODO(), name, "1"},
		}
		testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

		cancel()

		if !ps.IsComplete() {
			t.Errorf("expecting pipestream to be complete because fifo closed")
		}
	})(t)
}

func TestPipeStreamReadCompletedBecauseCancel(t *testing.T) {
	testutil.TimeoutTest(1*time.Second, func(t *testing.T) { // nolint:thelper
		var wg sync.WaitGroup

		tmpDir := testutil.TestTempDir(t)

		name := filepath.Join(tmpDir, "fifo")
		testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))

		lines := make(chan *logline.LogLine, 1)
		ctx, cancel := context.WithCancel(context.Background())
		waker, awaken := waker.NewTest(ctx, 1)

		f, err := os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
		testutil.FatalIfErr(t, err)

		ps, err := logstream.New(ctx, &wg, waker, name, lines, false)
		testutil.FatalIfErr(t, err)

		testutil.WriteString(t, f, "1\n")

		// Avoid a race with cancellation if we can synchronise with waker.Wake()
		awaken(0)

		cancel() // Cancellation here should cause the stream to shut down.

		wg.Wait()
		close(lines)

		received := testutil.LinesReceived(lines)
		expected := []*logline.LogLine{
			{context.TODO(), name, "1"},
		}
		testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

		if !ps.IsComplete() {
			t.Errorf("expecting pipestream to be complete because cancelled")
		}
	})(t)
}

func TestPipeStreamReadURL(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "fifo")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker := waker.NewTestAlways()

	ps, err := logstream.New(ctx, &wg, waker, "file://"+name, lines, false)
	testutil.FatalIfErr(t, err)

	f, err := os.OpenFile(name, os.O_WRONLY, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)
	testutil.WriteString(t, f, "1\n")

	// Pipes need to be closed to signal to the pipeStream to finish up.
	testutil.FatalIfErr(t, f.Close())

	ps.Stop() // no-op for pipes
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()

	if !ps.IsComplete() {
		t.Errorf("expecting pipestream to be complete because fifo closed")
	}
}
