// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

func makeTestTail(t *testing.T, options ...Option) (*Tailer, *stubProcessor, func(), string, func()) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)

	llp := NewStubProcessor()
	waker, awaken := waker.NewTest(1)
	ctx, cancel := context.WithCancel(context.Background())
	opts := append(options, LogstreamPollWaker(waker))
	ta, err := New(ctx, llp, opts...)
	testutil.FatalIfErr(t, err)
	return ta, llp, awaken, tmpDir, func() { cancel(); rmTmpDir() }
}

func TestTail(t *testing.T) {
	ta, _, _, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	defer f.Close()

	err := ta.TailPath(logfile)
	testutil.FatalIfErr(t, err)

	if _, ok := ta.logstreams[logfile]; !ok {
		t.Errorf("path not found in files map: %+#v", ta.logstreams)
	}
}

func TestHandleLogUpdateFullLine(t *testing.T) {
	ta, llp, awaken, dir, cleanup := makeTestTail(t, OneShot)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)

	testutil.FatalIfErr(t, ta.TailPath(logfile))

	llp.Add(4)
	testutil.WriteString(t, f, "a\nb\nc\nd\n")
	awaken()

	llp.Wait()

	expected := []*logline.LogLine{
		{context.Background(), logfile, "a"},
		{context.Background(), logfile, "b"},
		{context.Background(), logfile, "c"},
		{context.Background(), logfile, "d"},
	}
	testutil.ExpectNoDiff(t, expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

// TestHandleLogTruncate writes to a file, waits for those
// writes to be seen, then truncates the file and writes some more.
// At the end all lines written must be reported by the tailer.
func TestHandleLogTruncate(t *testing.T) {
	ta, llp, awaken, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)

	testutil.FatalIfErr(t, ta.TailPath(logfile))
	awaken()

	llp.Add(3)
	testutil.WriteString(t, f, "a\nb\nc\n")
	awaken()
	llp.Wait()

	if err := f.Truncate(0); err != nil {
		t.Fatal(err)
	}
	// "File.Truncate" does not change the file offset, force a seek to start.
	_, err := f.Seek(0, 0)
	testutil.FatalIfErr(t, err)
	awaken()

	llp.Add(2)
	testutil.WriteString(t, f, "d\ne\n")
	awaken()

	llp.Wait()

	expected := []*logline.LogLine{
		{context.Background(), logfile, "a"},
		{context.Background(), logfile, "b"},
		{context.Background(), logfile, "c"},
		{context.Background(), logfile, "d"},
		{context.Background(), logfile, "e"},
	}
	testutil.ExpectNoDiff(t, expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

func TestHandleLogUpdatePartialLine(t *testing.T) {
	ta, llp, awaken, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	llp.Add(1)

	testutil.FatalIfErr(t, ta.TailPath(logfile))
	awaken() // ensure we've hit an EOF before writing starts

	testutil.WriteString(t, f, "a")
	awaken()

	testutil.WriteString(t, f, "b")
	awaken()

	testutil.WriteString(t, f, "\n")
	awaken()

	llp.Wait()

	expected := []*logline.LogLine{
		{context.Background(), logfile, "ab"},
	}
	testutil.ExpectNoDiff(t, expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

func TestTailerOpenRetries(t *testing.T) {
	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	ta, llp, awaken, dir, cleanup := makeTestTail(t, OneShot)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	if _, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0); err != nil {
		t.Fatal(err)
	}

	llp.Add(1) // lines written
	testutil.FatalIfErr(t, ta.AddPattern(logfile))

	if err := ta.TailPath(logfile); err == nil || !os.IsPermission(err) {
		t.Fatalf("Expected a permission denied error here: %s", err)
	}
	testutil.FatalIfErr(t, ta.Poll())
	glog.Info("remove")
	if err := os.Remove(logfile); err != nil {
		t.Fatal(err)
	}
	testutil.FatalIfErr(t, ta.Poll())
	glog.Info("openfile")
	f, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0)
	testutil.FatalIfErr(t, err)
	testutil.FatalIfErr(t, ta.Poll())
	glog.Info("chmod")
	if err := os.Chmod(logfile, 0666); err != nil {
		t.Fatal(err)
	}
	testutil.FatalIfErr(t, ta.Poll())
	glog.Info("write string")
	testutil.WriteString(t, f, "\n")
	awaken()

	llp.Wait()
}

func TestHandleLogRotate(t *testing.T) {
	ta, llp, awaken, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)

	testutil.FatalIfErr(t, ta.TailPath(logfile))
	awaken()
	llp.Add(2)
	testutil.WriteString(t, f, "1\n")
	glog.Info("update")
	awaken()
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(logfile, logfile+".1"); err != nil {
		t.Fatal(err)
	}
	glog.Info("rename")
	awaken() // force an awaken to ensure we handle this moment in the rotation correctly
	f = testutil.TestOpenFile(t, logfile)
	glog.Info("create")
	awaken()
	testutil.WriteString(t, f, "2\n")
	glog.Info("update")
	awaken()

	llp.Wait()

	expected := []*logline.LogLine{
		{context.Background(), logfile, "1"},
		{context.Background(), logfile, "2"},
	}
	testutil.ExpectNoDiff(t, expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

func TestTailExpireStaleHandles(t *testing.T) {
	t.Skip("need to set lastRead on logstream to inject condition")
	ta, llp, awaken, dir, cleanup := makeTestTail(t)
	defer cleanup()

	log1 := filepath.Join(dir, "log1")
	f1 := testutil.TestOpenFile(t, log1)
	log2 := filepath.Join(dir, "log2")
	f2 := testutil.TestOpenFile(t, log2)

	if err := ta.TailPath(log1); err != nil {
		t.Fatal(err)
	}
	if err := ta.TailPath(log2); err != nil {
		t.Fatal(err)
	}
	llp.Add(2)
	testutil.WriteString(t, f1, "1\n")
	testutil.WriteString(t, f2, "2\n")
	awaken()
	llp.Wait()
	if err := ta.Gc(); err != nil {
		t.Fatal(err)
	}
	ta.logstreamsMu.RLock()
	if len(ta.logstreams) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.logstreams)
	}
	ta.logstreamsMu.RUnlock()
	ta.logstreamsMu.Lock()
	//ta.logstreams[log1].(*File).lastRead = time.Now().Add(-time.Hour*24 + time.Minute)
	ta.logstreamsMu.Unlock()
	if err := ta.Gc(); err != nil {
		t.Fatal(err)
	}
	ta.logstreamsMu.RLock()
	if len(ta.logstreams) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.logstreams)
	}
	ta.logstreamsMu.RUnlock()
	ta.logstreamsMu.Lock()
	//ta.logstreams[log1].(*File).lastRead = time.Now().Add(-time.Hour*24 - time.Minute)
	ta.logstreamsMu.Unlock()
	if err := ta.Gc(); err != nil {
		t.Fatal(err)
	}
	ta.logstreamsMu.RLock()
	if len(ta.logstreams) != 1 {
		t.Errorf("expecting 1 logstreams, got %v", ta.logstreams)
	}
	ta.logstreamsMu.RUnlock()
	glog.Info("good")
}
