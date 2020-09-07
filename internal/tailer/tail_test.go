// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

func makeTestTail(t *testing.T) (*Tailer, *stubProcessor, *watcher.FakeWatcher, string, func()) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)

	w := watcher.NewFakeWatcher()
	llp := NewStubProcessor()
	ta, err := New(llp, w, Context(context.Background()))
	if err != nil {
		t.Fatal(err)
	}
	return ta, llp, w, tmpDir, rmTmpDir
}

func TestTail(t *testing.T) {
	ta, _, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	defer f.Close()
	defer w.Close()

	err := ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}
	// Tail also causes the log to be read, so no need to inject an event.

	if _, ok := ta.handles[logfile]; !ok {
		t.Errorf("path not found in files map: %+#v", ta.handles)
	}
}

func TestHandleLogUpdate(t *testing.T) {
	ta, llp, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)

	err := ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}

	llp.Add(4)
	testutil.WriteString(t, f, "a\nb\nc\nd\n")
	// f.Seek(0, 0)
	w.InjectUpdate(logfile)

	llp.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}

	expected := []*logline.LogLine{
		{context.Background(), logfile, "a"},
		{context.Background(), logfile, "b"},
		{context.Background(), logfile, "c"},
		{context.Background(), logfile, "d"},
	}
	if diff := testutil.Diff(expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context")); diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}
}

// TestHandleLogTruncate writes to a file, waits for those
// writes to be seen, then truncates the file and writes some more.
// At the end all lines written must be reported by the tailer.
func TestHandleLogTruncate(t *testing.T) {
	ta, llp, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)

	if err := ta.TailPath(logfile); err != nil {
		t.Fatal(err)
	}

	llp.Add(3)
	testutil.WriteString(t, f, "a\nb\nc\n")
	w.InjectUpdate(logfile)
	llp.Wait()

	if err := f.Truncate(0); err != nil {
		t.Fatal(err)
	}
	// "File.Truncate" does not change the file offset.
	_, err := f.Seek(0, 0)
	testutil.FatalIfErr(t, err)
	w.InjectUpdate(logfile)

	llp.Add(2)
	testutil.WriteString(t, f, "d\ne\n")
	w.InjectUpdate(logfile)

	llp.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}

	expected := []*logline.LogLine{
		{context.Background(), logfile, "a"},
		{context.Background(), logfile, "b"},
		{context.Background(), logfile, "c"},
		{context.Background(), logfile, "d"},
		{context.Background(), logfile, "e"},
	}
	if diff := testutil.Diff(expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context")); diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}
}

func TestHandleLogUpdatePartialLine(t *testing.T) {
	ta, llp, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	llp.Add(1)

	err := ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}

	testutil.WriteString(t, f, "a")
	//f.Seek(0, 0)
	w.InjectUpdate(logfile)

	//f.Seek(1, 0)
	testutil.WriteString(t, f, "b")
	if err != nil {
		t.Error(err)
	}
	// f.Seek(1, 0)
	w.InjectUpdate(logfile)

	//f.Seek(2, 0)
	testutil.WriteString(t, f, "\n")
	//f.Seek(2, 0)
	w.InjectUpdate(logfile)

	llp.Wait()
	w.Close()

	expected := []*logline.LogLine{
		{context.Background(), logfile, "ab"},
	}
	diff := testutil.Diff(expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
	if diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}

}

func TestTailerOpenRetries(t *testing.T) {
	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	ta, llp, w, dir, cleanup := makeTestTail(t)
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
	//w.InjectUpdate(logfile)
	glog.Info("remove")
	if err := os.Remove(logfile); err != nil {
		t.Fatal(err)
	}
	w.InjectDelete(logfile)
	glog.Info("openfile")
	f, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0)
	if err != nil {
		t.Fatal(err)
	}
	w.InjectCreate(logfile)
	glog.Info("chmod")
	if err := os.Chmod(logfile, 0666); err != nil {
		t.Fatal(err)
	}
	w.InjectUpdate(logfile)
	glog.Info("write string")
	testutil.WriteString(t, f, "\n")
	w.InjectUpdate(logfile)

	llp.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
}

func TestTailerInitErrors(t *testing.T) {
	_, err := New(nil, nil, nil)
	if err == nil {
		t.Error("expected error")
	}
	lines := &stubProcessor{}
	_, err = New(lines, nil, nil)
	if err == nil {
		t.Error("expected error")
	}
	_, err = New(lines, nil)
	if err == nil {
		t.Error("expected error")
	}
	w := watcher.NewFakeWatcher()
	_, err = New(lines, w)
	if err != nil {
		t.Errorf("unexpected error %s", err)
	}
	_, err = New(lines, w, OneShot)
	if err != nil {
		t.Errorf("unexpected error %s", err)
	}
}

func TestHandleLogRotate(t *testing.T) {
	ta, llp, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)

	if err := ta.TailPath(logfile); err != nil {
		t.Fatal(err)
	}
	llp.Add(2)
	testutil.WriteString(t, f, "1\n")
	glog.V(2).Info("update")
	w.InjectUpdate(logfile)
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(logfile, logfile+".1"); err != nil {
		t.Fatal(err)
	}
	glog.V(2).Info("delete")
	w.InjectDelete(logfile)
	w.InjectCreate(logfile + ".1")
	f = testutil.TestOpenFile(t, logfile)
	glog.V(2).Info("create")
	w.InjectCreate(logfile)
	testutil.WriteString(t, f, "2\n")
	glog.V(2).Info("update")
	w.InjectUpdate(logfile)

	llp.Wait()
	w.Close()

	expected := []*logline.LogLine{
		{context.Background(), logfile, "1"},
		{context.Background(), logfile, "2"},
	}
	diff := testutil.Diff(expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
	if diff != "" {
		t.Errorf("result didn't match expected:\n%s", diff)
	}
}

func TestHandleLogRotateSignalsWrong(t *testing.T) {
	ta, llp, w, dir, cleanup := makeTestTail(t)
	defer cleanup()
	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)

	if err := ta.TailPath(logfile); err != nil {
		t.Fatal(err)
	}
	llp.Add(2)
	testutil.WriteString(t, f, "1\n")
	glog.V(2).Info("update")
	w.InjectUpdate(logfile)
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(logfile, logfile+".1"); err != nil {
		t.Fatal(err)
	}
	// No delete signal yet
	f = testutil.TestOpenFile(t, logfile)
	glog.V(2).Info("create")
	w.InjectCreate(logfile)

	glog.V(2).Info("delete")
	w.InjectDelete(logfile)

	testutil.WriteString(t, f, "2\n")
	glog.V(2).Info("update")
	w.InjectUpdate(logfile)

	llp.Wait()
	w.Close()

	expected := []*logline.LogLine{
		{context.Background(), logfile, "1"},
		{context.Background(), logfile, "2"},
	}
	diff := testutil.Diff(expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
	if diff != "" {
		t.Errorf("result didn't match expected:\n%s", diff)
	}
}

func TestTailExpireStaleHandles(t *testing.T) {
	ta, llp, w, dir, cleanup := makeTestTail(t)
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
	w.InjectUpdate(log1)
	w.InjectUpdate(log2)
	llp.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	if err := ta.Gc(); err != nil {
		t.Fatal(err)
	}
	ta.handlesMu.RLock()
	if len(ta.handles) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.handles)
	}
	ta.handlesMu.RUnlock()
	ta.handlesMu.Lock()
	ta.handles[log1].(*File).lastRead = time.Now().Add(-time.Hour*24 + time.Minute)
	ta.handlesMu.Unlock()
	if err := ta.Gc(); err != nil {
		t.Fatal(err)
	}
	ta.handlesMu.RLock()
	if len(ta.handles) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.handles)
	}
	ta.handlesMu.RUnlock()
	ta.handlesMu.Lock()
	ta.handles[log1].(*File).lastRead = time.Now().Add(-time.Hour*24 - time.Minute)
	ta.handlesMu.Unlock()
	if err := ta.Gc(); err != nil {
		t.Fatal(err)
	}
	ta.handlesMu.RLock()
	if len(ta.handles) != 1 {
		t.Errorf("expecting 1 handles, got %v", ta.handles)
	}
	ta.handlesMu.RUnlock()
	glog.Info("good")
}
