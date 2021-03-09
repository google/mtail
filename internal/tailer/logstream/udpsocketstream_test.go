// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"net"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

func TestUdpSocketStreamReadBsdSyslog(t *testing.T) {
	var wg sync.WaitGroup

	name := "udp:localhost:65111"

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	ss, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)
	awaken(1) // Synchronise past socket creation

	s, err := net.Dial("udp", "localhost:65111")
	testutil.FatalIfErr(t, err)

	// BSD syslog observed to trail with a space character
	_, err = s.Write([]byte("1 "))
	testutil.FatalIfErr(t, err)
	awaken(1)

	ss.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		// BSD syslog set to ignore trailing spaces
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context", "Filename"))

	cancel()
	wg.Wait()

	if !ss.IsComplete() {
		t.Errorf("expecting socketstream to be complete because cancellation")
	}
}

func TestUdpSocketStreamRead(t *testing.T) {
	var wg sync.WaitGroup

	name := "udp:localhost:65111"

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1)

	ss, err := logstream.New(ctx, &wg, waker, name, lines, false)
	testutil.FatalIfErr(t, err)
	awaken(1) // Synchronise past socket creation

	s, err := net.Dial("udp", "localhost:65111")
	testutil.FatalIfErr(t, err)

	_, err = s.Write([]byte("1\n"))
	testutil.FatalIfErr(t, err)
	awaken(1)

	ss.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		// Datagrams are our delimiters here, newline is unexpected,
		// but possible.
		{context.TODO(), name, "1\n"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context", "Filename"))

	cancel()
	wg.Wait()

	if !ss.IsComplete() {
		t.Errorf("expecting socketstream to be complete because cancellation")
	}
}
