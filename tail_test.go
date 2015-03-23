// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"os"
	"reflect"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/watcher"

	"github.com/spf13/afero"
)

func TestTail(t *testing.T) {
	fs := &afero.MemMapFs{}
	fs.Mkdir("tail_test", os.ModePerm)
	logfile := "/tmp/log"
	f, err := fs.Create(logfile)
	if err != nil {
		t.Error(err)
	}
	defer f.Close()

	w := watcher.NewFakeWatcher()
	lines := make(chan string)
	ta := NewTailer(lines, w, fs)
	if ta == nil {
		t.Fatalf("Couldn't make a tailer.")
	}
	defer ta.Stop()
	ta.Tail(logfile)
	// Tail also causes the log to be read, so no need to inject an event.

	if _, ok := ta.files[logfile]; !ok {
		t.Errorf("path not found in files map: %+#v", ta.files)
	}
}

func TestHandleLogUpdate(t *testing.T) {
	fs := &afero.MemMapFs{}
	err := fs.Mkdir("/tail_test", os.ModePerm)
	if err != nil {
		t.Fatalf("err: %s", err)
	}
	logfile := "/tail_test/log"
	f, err := fs.Create(logfile)
	if err != nil {
		t.Fatalf("err: %s", err)
	}

	lines := make(chan string)
	result := []string{}
	done := make(chan struct{})
	go func() {
		for line := range lines {
			glog.Infof("line: %q\n", line)
			result = append(result, line)
		}
		close(done)
	}()

	w := watcher.NewFakeWatcher()
	ta := NewTailer(lines, w, fs)
	if ta == nil {
		t.Fatalf("Couldn't make a tailer.")
	}

	ta.Tail(logfile)

	_, err = f.WriteString("a\nb\nc\nd\n")
	if err != nil {
		t.Fatal(err)
	}
	f.Seek(0, 0) // In memory files share the same offset

	w.InjectUpdate(logfile)

	// ugh
	time.Sleep(1 * time.Millisecond)

	ta.Stop()
	t.Logf("waiting")
	<-done

	expected := []string{"a", "b", "c", "d"}
	t.Logf("result: %v", result)
	if !reflect.DeepEqual(expected, result) {
		t.Errorf("result didn't match:\n\texpected: %v\n\treceived: %v", expected, result)
	}
}

func TestHandleLogChangePartialLine(t *testing.T) {
	fs := &afero.MemMapFs{}
	err := fs.Mkdir("/tail_test", os.ModePerm)
	if err != nil {
		t.Fatalf("err: %s", err)
	}
	logfile := "/tail_test/log"
	f, err := fs.Create(logfile)
	if err != nil {
		t.Fatalf("err: %s", err)
	}

	w := watcher.NewFakeWatcher()
	lines := make(chan string)
	ta := NewTailer(lines, w, fs)
	if ta == nil {
		t.Fatalf("Couldn't make a tailer.")
	}
	defer ta.Stop()
	ta.Tail(logfile)

	_, err = f.WriteString("a")
	if err != nil {
		t.Error(err)
	}
	w.InjectUpdate(logfile)
	select {
	case line := <-ta.lines:
		t.Errorf("unexpected line found: %s", line)
	case <-time.After(10 * time.Millisecond):
	}

	_, err = f.WriteString("b")
	if err != nil {
		t.Error(err)
	}
	w.InjectUpdate(logfile)

	select {
	case line := <-ta.lines:
		t.Errorf("unexpected line found: %s", line)
	case <-time.After(10 * time.Millisecond):
	}

	_, err = f.WriteString("\n")
	if err != nil {
		t.Error(err)
	}
	w.InjectUpdate(logfile)
	select {
	case line := <-ta.lines:
		expected := "ab"
		if line != expected {
			t.Errorf("line doesn't match: expected %q received %q", expected, line)
		}
	case <-time.After(10 * time.Millisecond):
		t.Errorf("no line read")
	}
}
