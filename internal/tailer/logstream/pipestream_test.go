// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"golang.org/x/sys/unix"
)

func TestPipeStreamRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "fifo")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0666))

	f, err := os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)

	sp := NewStubProcessor()

	ctx, cancel := context.WithCancel(context.Background())
	ps, err := logstream.New(ctx, &wg, name, sp)
	testutil.FatalIfErr(t, err)

	sp.ExpectLinesReceived(1)
	testutil.WriteString(t, f, "1\n")
	ps.Wake()

	sp.Verify()
	expected := []logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, sp.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()

	if !ps.IsFinished() {
		t.Errorf("expecting pipestream to be finished because cancellation")
	}
}

func TestPipeStreamFinishedBecauseClosed(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "fifo")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0666))

	f, err := os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)

	sp := NewStubProcessor()

	ctx, cancel := context.WithCancel(context.Background())
	ps, err := logstream.New(ctx, &wg, name, sp)
	testutil.FatalIfErr(t, err)

	sp.ExpectLinesReceived(1)
	testutil.WriteString(t, f, "1\n")
	ps.Wake()

	testutil.FatalIfErr(t, f.Close())
	ps.Wake()

	sp.Verify()
	expected := []logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, sp.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()

	if !ps.IsFinished() {
		t.Errorf("expecting pipestream to be finished because fifo closed")
	}
}
