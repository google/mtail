// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"bytes"
	"io"
	"os"
	"sync"
	"testing"

	"github.com/go-test/deep"
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

	if _, ok := ta.files[logfile]; !ok {
		t.Errorf("path not found in files map: %+#v", ta.files)
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

	_, err = f.WriteString("a\nb\nc\nd\n")
	if err != nil {
		t.Fatal(err)
	}
	f.Seek(0, 0) // In memory files share the same offset
	wg.Add(4)
	w.InjectUpdate(logfile)

	// ugh
	wg.Wait()
	w.Close()
	<-done

	expected := []*LogLine{
		&LogLine{logfile, "a"},
		&LogLine{logfile, "b"},
		&LogLine{logfile, "c"},
		&LogLine{logfile, "d"},
	}
	if diff := deep.Equal(result, expected); diff != nil {
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
		&LogLine{logfile, "ab"},
	}
	diff := deep.Equal(result, expected)
	if diff != nil {
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
	f.Seek(-1, os.SEEK_END)
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

	err = ta.TailFile(r)
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
