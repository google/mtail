// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix
// +build unix

package logstream_test

import (
	"context"
	"fmt"
	"net"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

func TestSocketStreamReadCompletedBecauseSocketClosed(t *testing.T) {
	for _, scheme := range []string{"unix", "tcp"} {
		scheme := scheme
		t.Run(scheme, testutil.TimeoutTest(time.Second, func(t *testing.T) { //nolint:thelper
			var wg sync.WaitGroup

			var addr string
			switch scheme {
			case "unix":
				tmpDir := testutil.TestTempDir(t)
				addr = filepath.Join(tmpDir, "sock")
			case "tcp":
				addr = fmt.Sprintf("[::]:%d", testutil.FreePort(t))
			default:
				t.Fatalf("bad scheme %s", scheme)
			}
			lines := make(chan *logline.LogLine, 1)
			ctx, cancel := context.WithCancel(context.Background())
			waker, awaken := waker.NewTest(ctx, 1)

			sockName := scheme + "://" + addr
			ss, err := logstream.New(ctx, &wg, waker, sockName, lines, false)
			testutil.FatalIfErr(t, err)

			s, err := net.Dial(scheme, addr)
			testutil.FatalIfErr(t, err)

			_, err = s.Write([]byte("1\n"))
			testutil.FatalIfErr(t, err)

			awaken(0) // Sync past read

			// Close the socket to signal to the socketStream to shut down.
			testutil.FatalIfErr(t, s.Close())

			ss.Stop() // stop after connection closes

			wg.Wait()
			close(lines)

			received := testutil.LinesReceived(lines)
			expected := []*logline.LogLine{
				{context.TODO(), addr, "1"},
			}
			testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

			cancel()

			if !ss.IsComplete() {
				t.Errorf("expecting socketstream to be complete because socket closed")
			}
		}))
	}
}

func TestSocketStreamReadCompletedBecauseCancel(t *testing.T) {
	for _, scheme := range []string{"unix", "tcp"} {
		scheme := scheme
		t.Run(scheme, testutil.TimeoutTest(time.Second, func(t *testing.T) { //nolint:thelper
			var wg sync.WaitGroup

			var addr string
			switch scheme {
			case "unix":
				tmpDir := testutil.TestTempDir(t)
				addr = filepath.Join(tmpDir, "sock")
			case "tcp":
				addr = fmt.Sprintf("[::]:%d", testutil.FreePort(t))
			default:
				t.Fatalf("bad scheme %s", scheme)
			}
			lines := make(chan *logline.LogLine, 1)
			ctx, cancel := context.WithCancel(context.Background())
			waker, awaken := waker.NewTest(ctx, 1)

			sockName := scheme + "://" + addr
			ss, err := logstream.New(ctx, &wg, waker, sockName, lines, false)
			testutil.FatalIfErr(t, err)

			s, err := net.Dial(scheme, addr)
			testutil.FatalIfErr(t, err)

			_, err = s.Write([]byte("1\n"))
			testutil.FatalIfErr(t, err)

			awaken(0) // Sync past read to ensure we read

			cancel() // This cancellation should cause the stream to shut down immediately.

			wg.Wait()
			close(lines)

			received := testutil.LinesReceived(lines)
			expected := []*logline.LogLine{
				{context.TODO(), addr, "1"},
			}
			testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

			if !ss.IsComplete() {
				t.Errorf("expecting socketstream to be complete because cancel")
			}
		}))
	}
}
