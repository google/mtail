// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"net"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/tailer/waker"
	"github.com/google/mtail/internal/testutil"
)

func TestSocketStreamRead(t *testing.T) {
	t.Skip("logstream.New cannot stat a nonexistent socket")
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "sock")

	sp := NewStubProcessor()
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())
	ss, err := logstream.New(ctx, &wg, waker, name, sp)
	testutil.FatalIfErr(t, err)
	awaken() // Synchronise past socket creation

	s, err := net.DialUnix("unixgram", nil, &net.UnixAddr{name, "unixgram"})
	testutil.FatalIfErr(t, err)

	sp.ExpectLinesReceived(1)
	_, err = s.Write([]byte("1\n"))
	testutil.FatalIfErr(t, err)
	awaken()

	sp.Verify()
	expected := []logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, sp.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()

	if !ss.IsFinished() {
		t.Errorf("expecting socketstream to be finished because cancellation")
	}
}

func TestSocketStreamFinishedBecauseClose(t *testing.T) {
	t.Skip("logstream.New cannot stat a nonexistent socket")
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "sock")

	sp := NewStubProcessor()
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())
	ss, err := logstream.New(ctx, &wg, waker, name, sp)
	testutil.FatalIfErr(t, err)
	awaken() // Synchronise past socket creation

	s, err := net.DialUnix("unixgram", nil, &net.UnixAddr{name, "unixgram"})
	testutil.FatalIfErr(t, err)

	sp.ExpectLinesReceived(1)
	_, err = s.Write([]byte("1\n"))
	testutil.FatalIfErr(t, err)
	awaken()

	testutil.FatalIfErr(t, s.Close())
	awaken()

	sp.Verify()
	expected := []logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, sp.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()

	if !ss.IsFinished() {
		t.Errorf("expecting socketstream to be finished because cancellation")
	}
}
