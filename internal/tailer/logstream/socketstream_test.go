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
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

func TestSocketStreamRead(t *testing.T) {
	t.Skip("logstream.New cannot stat a nonexistent socket")
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "sock")

	lines := make(chan *logline.LogLine, 1)
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())
	ss, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)
	awaken(1) // Synchronise past socket creation

	s, err := net.DialUnix("unixgram", nil, &net.UnixAddr{name, "unixgram"})
	testutil.FatalIfErr(t, err)

	_, err = s.Write([]byte("1\n"))
	testutil.FatalIfErr(t, err)
	awaken(1)

	ss.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()

	if !ss.IsComplete() {
		t.Errorf("expecting socketstream to be complete because cancellation")
	}
}

func TestSocketStreamCompletedBecauseSocketClosed(t *testing.T) {
	t.Skip("logstream.New cannot stat a nonexistent socket")
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "sock")

	lines := make(chan *logline.LogLine, 1)
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())
	ss, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)
	awaken(1) // Synchronise past socket creation

	s, err := net.DialUnix("unixgram", nil, &net.UnixAddr{name, "unixgram"})
	testutil.FatalIfErr(t, err)

	_, err = s.Write([]byte("1\n"))
	testutil.FatalIfErr(t, err)
	awaken(1)

	testutil.FatalIfErr(t, s.Close())
	awaken(1)

	ss.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()

	if !ss.IsComplete() {
		t.Errorf("expecting socketstream to be complete because socket closed")
	}
}

func TestSocketStreamCompletedBecauseCancel(t *testing.T) {
	t.Skip("logstream.New cannot stat a nonexistent socket")
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "sock")

	lines := make(chan *logline.LogLine, 1)
	waker, awaken := waker.NewTest(1)

	ctx, cancel := context.WithCancel(context.Background())
	ss, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)
	awaken(1) // Synchronise past socket creation

	s, err := net.DialUnix("unixgram", nil, &net.UnixAddr{name, "unixgram"})
	testutil.FatalIfErr(t, err)

	_, err = s.Write([]byte("1\n"))
	testutil.FatalIfErr(t, err)
	awaken(1)

	cancel()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !ss.IsComplete() {
		t.Errorf("expecting socketstream to be complete because cancel")
	}
}
