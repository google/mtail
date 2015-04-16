// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"syscall"
	"testing"
	"time"
)

func TestLogWatcher(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}

	workdir, err := ioutil.TempDir("", "log_watcher_test")
	if err != nil {
		t.Fatalf("could not create temporary working directory: %s", err)
	}

	defer func() {
		err := os.RemoveAll(workdir)
		if err != nil {
			t.Fatalf("could not remove temp dir %s: %s:", workdir, err)
		}
	}()

	w, err := NewLogWatcher()
	if err != nil {
		t.Fatalf("couldn't create a watcher: %s\n", err)
	}
	defer w.Close()

	w.Add(workdir)
	f, err := os.Create(filepath.Join(workdir, "logfile"))
	if err != nil {
		t.Fatalf("couldn't make a logfile in temp dir: %s\n", err)
	}
	select {
	case e := <-w.Events():
		switch e := e.(type) {
		case CreateEvent:
			if e.Pathname != filepath.Join(workdir, "logfile") {
				t.Errorf("create doesn't match")
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
	case <-time.After(10 * time.Millisecond):
		t.Errorf("didn't receive create message")
	}
	f.WriteString("hi")
	f.Close()
	select {
	case e := <-w.Events():
		switch e := e.(type) {
		case UpdateEvent:
			if e.Pathname != filepath.Join(workdir, "logfile") {
				t.Errorf("update doesn't match")
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
	case <-time.After(10 * time.Millisecond):
		t.Errorf("didn't receive update message")
	}
	os.Chmod(filepath.Join(workdir, "logfile"), os.ModePerm)
	select {
	case e := <-w.Events():
		t.Errorf("no event expected, got %q", e)
	case <-time.After(10 * time.Millisecond):
	}
	os.Remove(filepath.Join(workdir, "logfile"))
	select {
	case e := <-w.Events():
		switch e := e.(type) {
		case DeleteEvent:
			if e.Pathname != filepath.Join(workdir, "logfile") {
				t.Errorf("delete doesn't match")
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
	case <-time.After(10 * time.Millisecond):
		t.Errorf("didn't receive delete message")
	}
}

// This test may be OS specific; possibly break it out to a file with build tags.
func TestNewLogWatcherError(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}

	var rLimit syscall.Rlimit
	if err := syscall.Getrlimit(syscall.RLIMIT_NOFILE, &rLimit); err != nil {
		t.Fatalf("coulnd't get rlimit: %s", err)
	}
	var zero syscall.Rlimit = rLimit
	zero.Cur = 0
	if err := syscall.Setrlimit(syscall.RLIMIT_NOFILE, &zero); err != nil {
		t.Fatalf("couldn't set rlimit: %s", err)
	}
	_, err := NewLogWatcher()
	if err == nil {
		t.Errorf("didn't fail as expected")
	}
	//t.Logf("expected error: %s", err)
	if err := syscall.Setrlimit(syscall.RLIMIT_NOFILE, &rLimit); err != nil {
		t.Fatalf("couldn't reset rlimit: %s", err)
	}
}

func TestLogWatcherAddError(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}

	workdir, err := ioutil.TempDir("", "log_watcher_test")
	if err != nil {
		t.Fatalf("could not create temporary working directory: %s", err)
	}

	defer func() {
		err := os.RemoveAll(workdir)
		if err != nil {
			t.Fatalf("could not remove temp dir %s: %s:", workdir, err)
		}
	}()

	w, err := NewLogWatcher()
	if err != nil {
		t.Fatalf("couldn't create a watcher: %s\n", err)
	}
	defer w.Close()

	filename := filepath.Join(workdir, "test")
	if _, err := os.Create(filename); err != nil {
		t.Fatalf("couldn't create file: %s", err)
	}
	if err := os.Chmod(filename, 0); err != nil {
		t.Fatalf("couldn't chmod file: %s", err)
	}
	err = w.Add(filename)
	if err == nil {
		t.Errorf("didn't fail to add file")
	}
	//t.Logf("error: %s", err)
	if err := os.Chmod(filename, 0777); err != nil {
		t.Fatalf("coulnd't reset file perms: %s", err)
	}
}
