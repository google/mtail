// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"fmt"
	"os"
	"os/user"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

func makeTestTail(t *testing.T) (*Tailer, chan *logline.LogLine, *watcher.FakeWatcher, string, func()) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)

	w := watcher.NewFakeWatcher()
	lines := make(chan *logline.LogLine, 1)
	ta, err := New(lines, w)
	if err != nil {
		t.Fatal(err)
	}
	return ta, lines, w, tmpDir, rmTmpDir
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
	ta, lines, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	result := []*logline.LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			glog.V(2).Infof("line %v", line)
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	err := ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}

	wg.Add(4)
	_, err = f.WriteString("a\nb\nc\nd\n")
	if err != nil {
		t.Fatal(err)
	}
	// f.Seek(0, 0)
	w.InjectUpdate(logfile)

	wg.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	<-done

	expected := []*logline.LogLine{
		{logfile, "a"},
		{logfile, "b"},
		{logfile, "c"},
		{logfile, "d"},
	}
	if diff := testutil.Diff(expected, result); diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}
}

// TestHandleLogTruncate writes to a file, waits for those
// writes to be seen, then truncates the file and writes some more.
// At the end all lines written must be reported by the tailer.
func TestHandleLogTruncate(t *testing.T) {
	ta, lines, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	result := []*logline.LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	if err := ta.TailPath(logfile); err != nil {
		t.Fatal(err)
	}

	wg.Add(3)
	if _, err := f.WriteString("a\nb\nc\n"); err != nil {
		t.Fatal(err)
	}
	//time.Sleep(10 * time.Millisecond)
	w.InjectUpdate(logfile)
	wg.Wait()

	if err := f.Truncate(0); err != nil {
		t.Fatal(err)
	}
	// "File.Truncate" does not change the file offset.
	f.Seek(0, 0)
	w.InjectUpdate(logfile)
	//time.Sleep(10 * time.Millisecond)

	wg.Add(2)
	if _, err := f.WriteString("d\ne\n"); err != nil {
		t.Fatal(err)
	}
	w.InjectUpdate(logfile)
	//time.Sleep(10 * time.Millisecond)

	wg.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	<-done

	expected := []*logline.LogLine{
		{logfile, "a"},
		{logfile, "b"},
		{logfile, "c"},
		{logfile, "d"},
		{logfile, "e"},
	}
	if diff := testutil.Diff(expected, result); diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}
}

func TestHandleLogUpdatePartialLine(t *testing.T) {
	ta, lines, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	result := []*logline.LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	wg.Add(1)
	go func() {
		for line := range lines {
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	err := ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}

	_, err = f.WriteString("a")
	if err != nil {
		t.Fatal(err)
	}
	//f.Seek(0, 0)
	w.InjectUpdate(logfile)

	//f.Seek(1, 0)
	_, err = f.WriteString("b")
	if err != nil {
		t.Error(err)
	}
	// f.Seek(1, 0)
	w.InjectUpdate(logfile)

	//f.Seek(2, 0)
	_, err = f.WriteString("\n")
	if err != nil {
		t.Error(err)
	}
	//f.Seek(2, 0)
	w.InjectUpdate(logfile)

	wg.Wait()
	w.Close()
	<-done

	expected := []*logline.LogLine{
		{logfile, "ab"},
	}
	diff := testutil.Diff(expected, result)
	if diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}

}

func TestTailerOpenRetries(t *testing.T) {
	// Can't force a permission denied error if run as root.
	u, err := user.Current()
	if err != nil {
		t.Skip(fmt.Sprintf("Couldn't determine current user id: %s", err))
	}
	if u.Uid == "0" {
		t.Skip("Skipping test when run as root")
	}
	ta, lines, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	if _, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0); err != nil {
		t.Fatal(err)
	}

	done := make(chan struct{})
	wg := sync.WaitGroup{}
	wg.Add(1) // lines written
	go func() {
		for range lines {
			wg.Done()
		}
		close(done)
	}()
	ta.AddPattern(logfile)

	if err := ta.TailPath(logfile); err == nil || !os.IsPermission(err) {
		t.Fatalf("Expected a permission denied error here: %s", err)
	}
	//w.InjectUpdate(logfile)
	//time.Sleep(10 * time.Millisecond)
	glog.Info("remove")
	if err := os.Remove(logfile); err != nil {
		t.Fatal(err)
	}
	w.InjectDelete(logfile)
	//time.Sleep(10 * time.Millisecond)
	glog.Info("openfile")
	f, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0)
	if err != nil {
		t.Fatal(err)
	}
	w.InjectCreate(logfile)
	//	time.Sleep(10 * time.Millisecond)
	glog.Info("chmod")
	if err := os.Chmod(logfile, 0666); err != nil {
		t.Fatal(err)
	}
	w.InjectUpdate(logfile)
	//time.Sleep(10 * time.Millisecond)
	glog.Info("write string")
	if _, err := f.WriteString("\n"); err != nil {
		t.Fatal(err)
	}
	w.InjectUpdate(logfile)

	wg.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	<-done
}

func TestTailerInitErrors(t *testing.T) {
	_, err := New(nil, nil, nil)
	if err == nil {
		t.Error("expected error")
	}
	lines := make(chan *logline.LogLine)
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
	ta, lines, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	result := []*logline.LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	if err := ta.TailPath(logfile); err != nil {
		t.Fatal(err)
	}
	wg.Add(2)
	if _, err := f.WriteString("1\n"); err != nil {
		t.Fatal(err)
	}
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
	if _, err := f.WriteString("2\n"); err != nil {
		t.Fatal(err)
	}
	glog.V(2).Info("update")
	w.InjectUpdate(logfile)

	wg.Wait()
	w.Close()
	<-done

	expected := []*logline.LogLine{
		{logfile, "1"},
		{logfile, "2"},
	}
	diff := testutil.Diff(expected, result)
	if diff != "" {
		t.Errorf("result didn't match expected:\n%s", diff)
	}
}

func TestHandleLogRotateSignalsWrong(t *testing.T) {
	ta, lines, w, dir, cleanup := makeTestTail(t)
	defer cleanup()
	logfile := filepath.Join(dir, "log")
	f := testutil.TestOpenFile(t, logfile)
	result := []*logline.LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	if err := ta.TailPath(logfile); err != nil {
		t.Fatal(err)
	}
	wg.Add(2)
	if _, err := f.WriteString("1\n"); err != nil {
		t.Fatal(err)
	}
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

	time.Sleep(1 * time.Millisecond)
	glog.V(2).Info("delete")
	w.InjectDelete(logfile)

	if _, err := f.WriteString("2\n"); err != nil {
		t.Fatal(err)
	}
	glog.V(2).Info("update")
	w.InjectUpdate(logfile)

	wg.Wait()
	w.Close()
	<-done

	expected := []*logline.LogLine{
		{logfile, "1"},
		{logfile, "2"},
	}
	diff := testutil.Diff(expected, result)
	if diff != "" {
		t.Errorf("result didn't match expected:\n%s", diff)
	}
}

func TestTailExpireStaleHandles(t *testing.T) {
	ta, lines, w, dir, cleanup := makeTestTail(t)
	defer cleanup()

	result := []*logline.LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			glog.V(2).Infof("line %v", line)
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

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
	wg.Add(2)
	if _, err := f1.WriteString("1\n"); err != nil {
		t.Fatal(err)
	}
	if _, err := f2.WriteString("2\n"); err != nil {
		t.Fatal(err)
	}
	w.InjectUpdate(log1)
	w.InjectUpdate(log2)
	wg.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	<-done
	if err := ta.Expire(); err != nil {
		t.Fatal(err)
	}
	ta.handlesMu.RLock()
	if len(ta.handles) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.handles)
	}
	ta.handlesMu.RUnlock()
	ta.handlesMu.Lock()
	ta.handles[log1].LastRead = time.Now().Add(-time.Hour*24 + time.Minute)
	ta.handlesMu.Unlock()
	ta.handlesMu.RLock()
	if len(ta.handles) != 2 {
		t.Errorf("expecting 2 handles, got %v", ta.handles)
	}
	ta.handlesMu.RUnlock()
	ta.handlesMu.Lock()
	ta.handles[log1].LastRead = time.Now().Add(-time.Hour*24 - time.Minute)
	ta.handlesMu.Unlock()
	if err := ta.Expire(); err != nil {
		t.Fatal(err)
	}
	ta.handlesMu.RLock()
	if len(ta.handles) != 1 {
		t.Errorf("expecting 1 handles, got %v", ta.handles)
	}
	ta.handlesMu.RUnlock()
	glog.Info("good")
}
