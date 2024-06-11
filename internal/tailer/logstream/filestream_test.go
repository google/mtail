// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

func TestFileStreamRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, name, lines, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)
	awaken(1, 1) // synchronise past first read

	testutil.WriteString(t, f, "yo\n")
	awaken(1, 1)

	cancel()
	wg.Wait()
	close(lines)
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "yo"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stopped")
	}
}

func TestFileStreamReadOneShot(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()
	testutil.WriteString(t, f, "yo\n")

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker := waker.NewTestAlways()
	fs, err := logstream.New(ctx, &wg, waker, name, lines, logstream.OneShotEnabled)
	testutil.FatalIfErr(t, err)

	wg.Wait()
	close(lines)
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "yo"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stopped")
	}
	cancel()
	wg.Wait()
}

func TestFileStreamReadNonSingleByteEnd(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, name, lines, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)
	awaken(1, 1)

	s := "a"
	for i := 0; i < 4094; i++ {
		s += "a"
	}

	s += "中"
	testutil.WriteString(t, f, s+"\n")
	awaken(1, 1)

	fs.Stop()
	wg.Wait()
	close(lines)
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: s},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stopped")
	}
	cancel()
	wg.Wait()
}

func TestStreamDoesntBreakOnCorruptRune(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, name, lines, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)
	awaken(1, 1)

	s := string([]byte{0xF1})
	// 0xF1 = 11110001 , a byte signaling the start of a unicode character that
	// is 4 bytes long.

	// The following characters are regular single-byte characters.
	// utf8.DecodeRune will expect bytes that start with a 1 after the initial
	// unicode character 0xF1. This will result in a RuneError.
	for i := 0; i < 100; i++ {
		s += "a"
	}

	testutil.WriteString(t, f, s+"\n")
	awaken(1, 1)

	fs.Stop()
	wg.Wait()
	close(lines)
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: s[1:]},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because stopped")
	}
	cancel()
	wg.Wait()
}

func TestFileStreamTruncation(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.OpenLogFile(t, name)
	defer f.Close()

	lines := make(chan *logline.LogLine, 3)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, name, lines, logstream.OneShotDisabled)
	// fs.Stop() is also called explicitly further down but a failed test
	// and early return would lead to the handle staying open
	defer fs.Stop()

	testutil.FatalIfErr(t, err)
	awaken(1, 1) // Synchronise past first read after seekToEnd

	testutil.WriteString(t, f, "1\n2\n")
	awaken(1, 1)
	testutil.FatalIfErr(t, f.Close())
	awaken(1, 1)
	f = testutil.OpenLogFile(t, name)
	defer f.Close()

	testutil.WriteString(t, f, "3\n")
	awaken(1, 1)

	fs.Stop()
	wg.Wait()
	close(lines)

	received := testutil.LinesReceived(lines)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "1"},
		{Context: context.TODO(), Filename: name, Line: "2"},
		{Context: context.TODO(), Filename: name, Line: "3"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()
}

func TestFileStreamPartialRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	lines := make(chan *logline.LogLine, 1)
	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := logstream.New(ctx, &wg, waker, name, lines, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)
	awaken(1, 1)

	testutil.WriteString(t, f, "yo")
	awaken(1, 1)

	testutil.WriteString(t, f, "\n")
	awaken(1, 1)

	cancel()
	wg.Wait()

	close(lines)
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "yo"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because cancellation")
	}
}

func TestFileStreamReadToEOFOnCancel(t *testing.T) {
	var wg sync.WaitGroup
	ctx, cancel := context.WithCancel(context.Background())

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	lines := make(chan *logline.LogLine, 2)
	waker, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := logstream.New(ctx, &wg, waker, name, lines, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)
	awaken(1, 1)

	testutil.WriteString(t, f, "line 1\n")
	awaken(1, 1)

	testutil.WriteString(t, f, "line 2\n")
	cancel() // cancel wakes the stream

	wg.Wait()

	close(lines) // Signal it's time to go.
	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "line 1"},
		{Context: context.TODO(), Filename: name, Line: "line 2"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because cancellation")
	}
}
