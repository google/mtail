// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix

package logstream_test

import (
	"context"
	"fmt"
	"net"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/tailer/logstream"
	"github.com/jaqx0r/mtail/internal/testutil"
	"github.com/jaqx0r/mtail/internal/waker"
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

			ctx, cancel := context.WithCancel(context.Background())
			// The stream is not shut down with cancel in this test.
			defer cancel()
			waker := waker.NewTestAlways()

			sockName := scheme + "://" + addr
			ss, err := logstream.New(ctx, &wg, waker, sockName, logstream.OneShotEnabled)
			testutil.FatalIfErr(t, err)

			expected := []*logline.LogLine{
				{Context: context.TODO(), Filename: sockName, Line: "1"},
			}
			checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, ss.Lines())

			s, err := net.Dial(scheme, addr)
			testutil.FatalIfErr(t, err)

			_, err = s.Write([]byte("1\n"))
			testutil.FatalIfErr(t, err)

			// Close the socket to signal to the socketStream to shut down.
			testutil.FatalIfErr(t, s.Close())

			wg.Wait()

			checkLineDiff()

			if v := <-ss.Lines(); v != nil {
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

			ctx, cancel := context.WithCancel(context.Background())
			waker := waker.NewTestAlways()

			sockName := scheme + "://" + addr
			ss, err := logstream.New(ctx, &wg, waker, sockName, logstream.OneShotDisabled)
			testutil.FatalIfErr(t, err)

			expected := []*logline.LogLine{
				{Context: context.TODO(), Filename: sockName, Line: "1"},
			}
			checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, ss.Lines())

			s, err := net.Dial(scheme, addr)
			testutil.FatalIfErr(t, err)

			_, err = s.Write([]byte("1\n"))
			testutil.FatalIfErr(t, err)

			// Yield to give the stream a chance to read.
			time.Sleep(10 * time.Millisecond)

			cancel() // This cancellation should cause the stream to shut down immediately.
			wg.Wait()

			checkLineDiff()

			if v := <-ss.Lines(); v != nil {
				t.Errorf("expecting socketstream to be complete because cancel")
			}
		}))
	}
}
