// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"bytes"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/go-cmp/cmp"
	"github.com/google/mtail/watcher"

	"github.com/spf13/afero"
)

func makeTestTail(t *testing.T) (*Tailer, chan *LogLine, *watcher.FakeWatcher, afero.Fs) {
	fs := afero.NewMemMapFs()
	w := watcher.NewFakeWatcher()
	lines := make(chan *LogLine, 1)
	o := Options{lines, false, w, fs}
	ta, err := New(o)
	if err != nil {
		t.Fatal(err)
	}
	return ta, lines, w, fs
}

func makeTestTailReal(t *testing.T, prefix string) (*Tailer, chan *LogLine, *watcher.LogWatcher, afero.Fs, string) {
	if testing.Short() {
		t.Skip("skipping real fs test in short mode")
	}
	dir, err := ioutil.TempDir("", prefix)
	if err != nil {
		t.Fatalf("can't create tempdir: %v", err)
	}

	fs := afero.NewOsFs()
	w, err := watcher.NewLogWatcher()
	if err != nil {
		t.Fatalf("can't create watcher: %v", err)
	}
	lines := make(chan *LogLine, 1)
	o := Options{lines, false, w, fs}
	ta, err := New(o)
	if err != nil {
		t.Fatal(err)
	}
	return ta, lines, w, fs, dir
}

func TestTail(t *testing.T) {
	ta, _, w, fs := makeTestTail(t)
	fs.Mkdir("tail_test", os.ModePerm)
	logfile := "/tmp/log"
	f, err := fs.Create(logfile)
	if err != nil {
		t.Error(err)
	}
	defer f.Close()
	defer w.Close()

	err = ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}
	// Tail also causes the log to be read, so no need to inject an event.

	if _, ok := ta.handles[logfile]; !ok {
		t.Errorf("path not found in files map: %+#v", ta.handles)
	}
}

func TestHandleLogUpdate(t *testing.T) {
	ta, lines, w, fs := makeTestTail(t)

	err := fs.Mkdir("/tail_test", os.ModePerm)
	if err != nil {
		t.Fatalf("err: %s", err)
	}
	logfile := "/tail_test/log"
	f, err := fs.Create(logfile)
	if err != nil {
		t.Fatalf("err: %s", err)
	}

	result := []*LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	err = ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}

	wg.Add(4)
	_, err = f.WriteString("a\nb\nc\nd\n")
	if err != nil {
		t.Fatal(err)
	}
	f.Seek(0, 0) // afero in-memory files share the same offset
	w.InjectUpdate(logfile)

	wg.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	<-done

	expected := []*LogLine{
		{logfile, "a"},
		{logfile, "b"},
		{logfile, "c"},
		{logfile, "d"},
	}
	if diff := cmp.Diff(expected, result); diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}
}

// TestHandleLogTruncate writes to a file, waits for those
// writes to be seen, then truncates the file and writes some more.
// At the end all lines written must be reported by the tailer.
func TestHandleLogTruncate(t *testing.T) {
	ta, lines, w, fs, dir := makeTestTailReal(t, "truncate")
	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Log(err)
		}
	}()

	logfile := filepath.Join(dir, "log")
	f, err := fs.Create(logfile)
	if err != nil {
		t.Fatalf("err: %s", err)
	}

	result := []*LogLine{}
	done := make(chan struct{})
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	if err = ta.TailPath(logfile); err != nil {
		t.Fatal(err)
	}

	wg.Add(3)
	if _, err = f.WriteString("a\nb\nc\n"); err != nil {
		t.Fatal(err)
	}
	time.Sleep(10 * time.Millisecond)
	wg.Wait()

	if err = f.Truncate(0); err != nil {
		t.Fatal(err)
	}
	// "File.Truncate" does not change the file offset.
	f.Seek(0, 0)
	time.Sleep(10 * time.Millisecond)

	wg.Add(2)
	if _, err = f.WriteString("d\ne\n"); err != nil {
		t.Fatal(err)
	}
	time.Sleep(10 * time.Millisecond)

	wg.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	<-done

	expected := []*LogLine{
		{logfile, "a"},
		{logfile, "b"},
		{logfile, "c"},
		{logfile, "d"},
		{logfile, "e"},
	}
	if diff := cmp.Diff(expected, result); diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}
}

func TestHandleLogUpdatePartialLine(t *testing.T) {
	ta, lines, w, fs := makeTestTail(t)

	err := fs.Mkdir("/tail_test", os.ModePerm)
	if err != nil {
		t.Fatalf("err: %s", err)
	}
	logfile := "/tail_test/log"
	f, err := fs.Create(logfile)
	if err != nil {
		t.Fatalf("err: %s", err)
	}

	result := []*LogLine{}
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

	err = ta.TailPath(logfile)
	if err != nil {
		t.Fatal(err)
	}

	_, err = f.WriteString("a")
	if err != nil {
		t.Fatal(err)
	}
	f.Seek(0, 0)
	w.InjectUpdate(logfile)

	f.Seek(1, 0)
	_, err = f.WriteString("b")
	if err != nil {
		t.Error(err)
	}
	f.Seek(1, 0)
	w.InjectUpdate(logfile)

	f.Seek(2, 0)
	_, err = f.WriteString("\n")
	if err != nil {
		t.Error(err)
	}
	f.Seek(2, 0)
	w.InjectUpdate(logfile)

	wg.Wait()
	w.Close()
	<-done

	expected := []*LogLine{
		{logfile, "ab"},
	}
	diff := cmp.Diff(expected, result)
	if diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}

}

func TestReadPartial(t *testing.T) {
	ta, lines, w, fs := makeTestTail(t)
	defer w.Close()

	f, err := fs.Create("t")
	if err != nil {
		t.Fatal(err)
	}
	p := bytes.NewBufferString("")
	err = ta.read(f, p)
	if p.String() != "" {
		t.Errorf("partial line returned not empty: %q", p)
	}
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	p.Reset()
	p.WriteString("o")
	f.WriteString("hi")
	f.Seek(0, 0)
	err = ta.read(f, p)
	if p.String() != "ohi" {
		t.Errorf("partial line returned not expected: %q", p)
	}
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	p.Reset()
	err = ta.read(f, p)
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	f.WriteString("\n")
	f.Seek(-1, io.SeekEnd)
	p.Reset()
	p.WriteString("ohi")
	err = ta.read(f, p)
	l := <-lines
	if l.Line != "ohi" {
		t.Errorf("line emitted not ohi: %q", l)
	}
	if p.String() != "" {
		t.Errorf("partial not empty: %q", p)
	}
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
}

func TestReadPipe(t *testing.T) {
	ta, lines, wa, _ := makeTestTail(t)
	defer wa.Close()

	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}

	err = ta.TailHandle(r)
	if err != nil {
		t.Fatal(err)
	}
	n, err := w.WriteString("hi\n")
	if err != nil {
		t.Fatal(err)
	}
	if n < 2 {
		t.Fatalf("Didn't write enough bytes: %d", n)
	}
	l := <-lines
	if l.Line != "hi" {
		t.Errorf("line not expected: %q", l)
	}
}

func TestOpenRetries(t *testing.T) {
	// Use the real filesystem because afero doesn't implement correct
	// permissions checking on OpenFile in the memfile implementation.
	ta, lines, w, fs, dir := makeTestTailReal(t, "retries")
	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Log(err)
		}
	}()

	logfile := filepath.Join(dir, "log")
	if _, err := fs.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0); err != nil {
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

	if err := ta.TailPath(logfile); err == nil || !os.IsPermission(err) {
		t.Fatalf("Expected a permission denied error here: %s", err)
	}
	time.Sleep(10 * time.Millisecond)
	glog.Info("remove")
	if err := fs.Remove(logfile); err != nil {
		t.Fatal(err)
	}
	time.Sleep(10 * time.Millisecond)
	glog.Info("openfile")
	f, err := fs.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0)
	if err != nil {
		t.Fatal(err)
	}
	time.Sleep(10 * time.Millisecond)
	glog.Info("chmod")
	if err := fs.Chmod(logfile, 0666); err != nil {
		t.Fatal(err)
	}
	time.Sleep(10 * time.Millisecond)
	glog.Info("write string")
	if _, err := f.WriteString("\n"); err != nil {
		t.Fatal(err)
	}
	wg.Wait()
	if err := w.Close(); err != nil {
		t.Log(err)
	}
	<-done
}

func TestTailerInitErrors(t *testing.T) {
	o := Options{}
	_, err := New(o)
	if err == nil {
		t.Error("expected error")
	}
	o.Lines = make(chan *LogLine)
	_, err = New(o)
	if err == nil {
		t.Error("expected error")
	}
	o.FS = afero.NewMemMapFs()
	_, err = New(o)
	if err == nil {
		t.Error("expected error")
	}
	o.W = watcher.NewFakeWatcher()
	_, err = New(o)
	if err != nil {
		t.Errorf("unexpected error %s", err)
	}
}
