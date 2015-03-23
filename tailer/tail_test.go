// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"os"
	"reflect"
	"sync"
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
	ta := New(lines, w, fs)
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
	wg := sync.WaitGroup{}
	go func() {
		for line := range lines {
			glog.Infof("line: %q\n", line)
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	w := watcher.NewFakeWatcher()
	ta := New(lines, w, fs)
	if ta == nil {
		t.Fatalf("Couldn't make a tailer.")
	}

	ta.Tail(logfile)

	_, err = f.WriteString("a\nb\nc\nd\n")
	if err != nil {
		t.Fatal(err)
	}
	f.Seek(0, 0) // In memory files share the same offset
	wg.Add(4)
	w.InjectUpdate(logfile)

	// ugh
	wg.Wait()

	ta.Stop()
	t.Logf("waiting")
	<-done

	expected := []string{"a", "b", "c", "d"}
	t.Logf("result: %v", result)
	if !reflect.DeepEqual(expected, result) {
		t.Errorf("result didn't match:\n\texpected: %v\n\treceived: %v", expected, result)
	}
}

func TestHandleLogUpdatePartialLine(t *testing.T) {
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
	wg := sync.WaitGroup{}
	wg.Add(1)
	go func() {
		for line := range lines {
			glog.Infof("line: %q\n", line)
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()

	w := watcher.NewFakeWatcher()
	ta := New(lines, w, fs)
	if ta == nil {
		t.Fatalf("Couldn't make a tailer.")
	}

	ta.Tail(logfile)

	_, err = f.WriteString("a")
	if err != nil {
		t.Fatal(err)
	}
	f.Seek(0, 0)
	w.InjectUpdate(logfile)
	// These sleeps are due to afero using the same data structure for both
	// this thread and the method under test, so the file offset is shared.  We
	// have to wait for the handleLogUpdate method to run and as yet there's no
	// signal that this occurred.
	time.Sleep(1 * time.Millisecond)

	_, err = f.WriteString("b")
	if err != nil {
		t.Error(err)
	}
	f.Seek(1, 0)
	w.InjectUpdate(logfile)
	time.Sleep(1 * time.Millisecond)

	_, err = f.WriteString("\n")
	if err != nil {
		t.Error(err)
	}
	f.Seek(2, 0)
	w.InjectUpdate(logfile)

	wg.Wait()
	ta.Stop()
	<-done

	expected := []string{"ab"}
	t.Logf("result: %v", result)
	if !reflect.DeepEqual(expected, result) {
		t.Errorf("result didn't match:\n\texpected: %v\n\treceived: %v", expected, result)
	}

}
