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

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "yo"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	awaken(1, 1) // synchronise past first read

	testutil.WriteString(t, f, "yo\n")
	awaken(1, 1)

	cancel()
	wg.Wait()

	checkLineDiff()

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

	ctx, cancel := context.WithCancel(context.Background())
	waker := waker.NewTestAlways()

	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotEnabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "yo"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	wg.Wait()

	checkLineDiff()

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

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	s := "a"
	for i := 0; i < 4094; i++ {
		s += "a"
	}
	s += "中"

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: s},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	awaken(1, 1)

	testutil.WriteString(t, f, s+"\n")
	awaken(1, 1)

	fs.Stop()
	wg.Wait()

	checkLineDiff()

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

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	s := string([]byte{0xF1})
	// 0xF1 = 11110001 , a byte signaling the start of a unicode character that
	// is 4 bytes long.

	// The following characters are regular single-byte characters.
	// utf8.DecodeRune will expect bytes that start with a 1 after the initial
	// unicode character 0xF1. This will result in a RuneError.
	for i := 0; i < 100; i++ {
		s += "a"
	}
	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: s[1:]},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	awaken(1, 1)

	testutil.WriteString(t, f, s+"\n")
	awaken(1, 1)

	fs.Stop()
	wg.Wait()

	checkLineDiff()

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

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
	// fs.Stop() is also called explicitly further down but a failed test
	// and early return would lead to the handle staying open
	defer fs.Stop()
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "1"},
		{Context: context.TODO(), Filename: name, Line: "2"},
		{Context: context.TODO(), Filename: name, Line: "3"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

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

	checkLineDiff()

	cancel()
	wg.Wait()
}

func TestFileStreamPartialRead(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir := testutil.TestTempDir(t)

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	defer f.Close()

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "yo"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	awaken(1, 1)

	testutil.WriteString(t, f, "yo")
	awaken(1, 1)

	testutil.WriteString(t, f, "\n")
	awaken(1, 1)

	cancel()
	wg.Wait()

	checkLineDiff()

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

	waker, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := logstream.New(ctx, &wg, waker, name, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: name, Line: "line 1"},
		{Context: context.TODO(), Filename: name, Line: "line 2"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	awaken(1, 1)

	testutil.WriteString(t, f, "line 1\n")
	awaken(1, 1)

	testutil.WriteString(t, f, "line 2\n")
	cancel() // cancel wakes the stream

	wg.Wait()

	checkLineDiff()

	if !fs.IsComplete() {
		t.Errorf("expecting filestream to be complete because cancellation")
	}
}
