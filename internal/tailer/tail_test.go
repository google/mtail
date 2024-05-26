// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

type testTail struct {
	*Tailer

	// Output lnes channel
	lines chan *logline.LogLine

	// Method to wake the stream poller
	awakenStreams waker.WakeFunc

	// Method to wake the pattern poller
	awakenPattern waker.WakeFunc

	// Temporary dir for test
	tmpDir string

	// Issue a shutdown to the test tailer.
	stop func()
}

func makeTestTail(t *testing.T, options ...Option) *testTail {
	t.Helper()
	tmpDir := testutil.TestTempDir(t)

	ctx, cancel := context.WithCancel(context.Background())
	lines := make(chan *logline.LogLine, 5) // 5 loglines ought to be enough for any test
	var wg sync.WaitGroup
	streamWaker, awakenStreams := waker.NewTest(ctx, 1, "streams")
	patternWaker, awakenPattern := waker.NewTest(ctx, 1, "pattern")
	options = append(options, LogPatterns([]string{tmpDir}), LogPatternPollWaker(patternWaker), LogstreamPollWaker(streamWaker))
	ta, err := New(ctx, &wg, lines, options...)
	testutil.FatalIfErr(t, err)
	stop := func() { cancel(); wg.Wait() }
	t.Cleanup(stop)
	return &testTail{Tailer: ta, lines: lines, awakenStreams: awakenStreams, awakenPattern: awakenPattern, tmpDir: tmpDir, stop: stop}
}

func TestNewErrors(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	lines := make(chan *logline.LogLine)
	var wg sync.WaitGroup
	_, err := New(ctx, nil, lines)
	if err == nil {
		t.Error("New(ctx, nil, lines) expecting error, received nil")
	}
	_, err = New(ctx, &wg, nil)
	if err == nil {
		t.Error("New(ctx, wg, nil) expecting error, received nil")
	}
}

func TestTailPath(t *testing.T) {
	ta := makeTestTail(t)

	logfile := filepath.Join(ta.tmpDir, "log")
	f := testutil.TestOpenFile(t, logfile)
	defer f.Close()

	err := ta.TailPath(logfile)
	testutil.FatalIfErr(t, err)

	if _, ok := ta.logstreams[logfile]; !ok {
		t.Errorf("path not found in files map: %+#v", ta.logstreams)
	}
}

func TestHandleLogUpdate(t *testing.T) {
	ta := makeTestTail(t)

	logfile := filepath.Join(ta.tmpDir, "log")
	f := testutil.TestOpenFile(t, logfile)
	defer f.Close()

	testutil.FatalIfErr(t, ta.TailPath(logfile))
	ta.awakenStreams(1, 1)

	testutil.WriteString(t, f, "a\nb\nc\nd\n")
	ta.awakenStreams(1, 1)

	ta.stop()

	received := testutil.LinesReceived(ta.lines)
	expected := []*logline.LogLine{
		{Context: context.Background(), Filename: logfile, Line: "a"},
		{Context: context.Background(), Filename: logfile, Line: "b"},
		{Context: context.Background(), Filename: logfile, Line: "c"},
		{Context: context.Background(), Filename: logfile, Line: "d"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

// TestHandleLogTruncate writes to a file, waits for those
// writes to be seen, then truncates the file and writes some more.
// At the end all lines written must be reported by the tailer.
func TestHandleLogTruncate(t *testing.T) {
	ta := makeTestTail(t)

	logfile := filepath.Join(ta.tmpDir, "log")
	f := testutil.OpenLogFile(t, logfile)
	defer f.Close()

	testutil.FatalIfErr(t, ta.TailPath(logfile))
	// Expect to wake 1 wakee, the logstream reading `logfile`.
	ta.awakenStreams(1, 1)

	testutil.WriteString(t, f, "a\nb\nc\n")
	ta.awakenStreams(1, 1)

	if err := f.Truncate(0); err != nil {
		t.Fatal(err)
	}

	// "File.Truncate" does not change the file offset, force a seek to start.
	_, err := f.Seek(0, 0)
	testutil.FatalIfErr(t, err)
	ta.awakenStreams(1, 1)

	testutil.WriteString(t, f, "d\ne\n")
	ta.awakenStreams(1, 1)

	ta.stop()

	received := testutil.LinesReceived(ta.lines)
	expected := []*logline.LogLine{
		{Context: context.Background(), Filename: logfile, Line: "a"},
		{Context: context.Background(), Filename: logfile, Line: "b"},
		{Context: context.Background(), Filename: logfile, Line: "c"},
		{Context: context.Background(), Filename: logfile, Line: "d"},
		{Context: context.Background(), Filename: logfile, Line: "e"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

func TestHandleLogUpdatePartialLine(t *testing.T) {
	ta := makeTestTail(t)

	logfile := filepath.Join(ta.tmpDir, "log")
	f := testutil.TestOpenFile(t, logfile)
	defer f.Close()

	testutil.FatalIfErr(t, ta.TailPath(logfile))
	ta.awakenStreams(1, 1) // ensure we've hit an EOF before writing starts

	testutil.WriteString(t, f, "a")
	ta.awakenStreams(1, 1)

	testutil.WriteString(t, f, "b")
	ta.awakenStreams(1, 1)

	testutil.WriteString(t, f, "\n")
	ta.awakenStreams(1, 1)

	ta.stop()

	received := testutil.LinesReceived(ta.lines)
	expected := []*logline.LogLine{
		{Context: context.Background(), Filename: logfile, Line: "ab"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

// Test that broken files are skipped.
func TestTailerUnreadableFile(t *testing.T) {
	t.Skip("race condition testing for the absence of a log")
	ta := makeTestTail(t)

	brokenlog := filepath.Join(ta.tmpDir, "brokenlog")
	goodlog := filepath.Join(ta.tmpDir, "goodlog")
	testutil.FatalIfErr(t, ta.AddPattern(brokenlog))
	testutil.FatalIfErr(t, ta.AddPattern(goodlog))

	logCountCheck := testutil.ExpectExpvarDeltaWithDeadline(t, "log_count", 1)

	glog.Info("create logs")
	testutil.FatalIfErr(t, os.Symlink("/nonexistent", brokenlog))
	f := testutil.TestOpenFile(t, goodlog)
	defer f.Close()

	// We started with one pattern waker from `makeTestTail`, but we added two
	// patterns. There's also the completion poller. Collect them all.
	ta.awakenPattern(1, 4)

	logCountCheck()
}

func TestTailerInitErrors(t *testing.T) {
	var wg sync.WaitGroup
	_, err := New(context.TODO(), &wg, nil)
	if err == nil {
		t.Error("expected error")
	}
	ctx, cancel := context.WithCancel(context.Background())
	_, err = New(ctx, &wg, nil, nil)
	if err == nil {
		t.Error("expected error")
	}
	lines := make(chan *logline.LogLine, 1)
	_, err = New(ctx, &wg, lines, nil)
	if err == nil {
		t.Error("expected error")
	}
	cancel()
	wg.Wait()
	lines = make(chan *logline.LogLine, 1)
	ctx, cancel = context.WithCancel(context.Background())
	_, err = New(ctx, &wg, lines)
	if err != nil {
		t.Errorf("unexpected error %s", err)
	}
	cancel()
	wg.Wait()
	lines = make(chan *logline.LogLine, 1)
	ctx, cancel = context.WithCancel(context.Background())
	_, err = New(ctx, &wg, lines, OneShot)
	if err != nil {
		t.Errorf("unexpected error %s", err)
	}
	cancel()
	wg.Wait()
}

func TestTailExpireStaleHandles(t *testing.T) {
	t.Skip("need to set lastRead on logstream to inject condition")
	ta := makeTestTail(t)

	log1 := filepath.Join(ta.tmpDir, "log1")
	f1 := testutil.TestOpenFile(t, log1)
	log2 := filepath.Join(ta.tmpDir, "log2")
	f2 := testutil.TestOpenFile(t, log2)

	if err := ta.TailPath(log1); err != nil {
		t.Fatal(err)
	}
	if err := ta.TailPath(log2); err != nil {
		t.Fatal(err)
	}
	testutil.WriteString(t, f1, "1\n")
	testutil.WriteString(t, f2, "2\n")

	ta.awakenStreams(1, 1)

	ta.stop()

	received := testutil.LinesReceived(ta.lines)
	expected := []*logline.LogLine{
		{Context: context.Background(), Filename: log1, Line: "1"},
		{Context: context.Background(), Filename: log2, Line: "2"},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	if err := ta.ExpireStaleLogstreams(); err != nil {
		t.Fatal(err)
	}
	ta.logstreamsMu.RLock()
	if len(ta.logstreams) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.logstreams)
	}
	ta.logstreamsMu.RUnlock()
	// ta.logstreamsMu.Lock()
	// ta.logstreams[log1].(*File).lastRead = time.Now().Add(-time.Hour*24 + time.Minute)
	// ta.logstreamsMu.Unlock()
	if err := ta.ExpireStaleLogstreams(); err != nil {
		t.Fatal(err)
	}
	ta.logstreamsMu.RLock()
	if len(ta.logstreams) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.logstreams)
	}
	ta.logstreamsMu.RUnlock()
	// ta.logstreamsMu.Lock()
	// ta.logstreams[log1].(*File).lastRead = time.Now().Add(-time.Hour*24 - time.Minute)
	// ta.logstreamsMu.Unlock()
	if err := ta.ExpireStaleLogstreams(); err != nil {
		t.Fatal(err)
	}
	ta.logstreamsMu.RLock()
	if len(ta.logstreams) != 1 {
		t.Errorf("expecting 1 logstreams, got %v", ta.logstreams)
	}
	ta.logstreamsMu.RUnlock()
	glog.Info("good")
}

func TestAddGlob(t *testing.T) {
	ta := makeTestTail(t)

	pattern := "foo?.log"
	err := ta.AddPattern(pattern)
	if err != nil {
		t.Errorf("AddPattern(%v) -> %v", pattern, err)
	}
	absPattern, err := filepath.Abs(pattern)
	testutil.FatalIfErr(t, err)
	ta.globPatternsMu.Lock()
	_, ok := ta.globPatterns[absPattern]
	if !ok {
		t.Errorf("pattern not found in globPatterns: %v", ta.globPatterns)
	}
	ta.globPatternsMu.Unlock()
}
