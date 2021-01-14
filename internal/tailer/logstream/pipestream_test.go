// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"syscall"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
	"golang.org/x/sys/unix"
)

func TestPipeStreamRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "fifo")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0666))

	f, err := os.OpenFile(name, os.O_WRONLY|syscall.O_NONBLOCK, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	ps, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)

	testutil.WriteString(t, f, "1\n")
	awaken(1)

	ps.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()

	if !ps.IsComplete() {
		t.Errorf("expecting pipestream to be complete because cancellation")
	}
}

func TestPipeStreamCompletedBecausePipeClosed(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "fifo")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0600))

	f, err := os.OpenFile(name, os.O_WRONLY|syscall.O_NONBLOCK, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	ps, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)

	testutil.WriteString(t, f, "1\n")
	awaken(1)

	testutil.FatalIfErr(t, f.Close())
	//awaken()

	cancel()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !ps.IsComplete() {
		t.Errorf("expecting pipestream to be complete because fifo closed")
	}
}

func TestPipeStreamCompletedBecauseCancel(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "fifo")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0666))

	f, err := os.OpenFile(name, os.O_WRONLY|syscall.O_NONBLOCK, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	ps, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)

	testutil.WriteString(t, f, "1\n")
	awaken(1)

	cancel()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !ps.IsComplete() {
		t.Errorf("expecting pipestream to be complete because stop called")
	}
}
