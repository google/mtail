// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"context"
	"os"
	"path"
	"path/filepath"
	"testing"
	"time"

	"github.com/google/mtail/internal/testutil"
)

type testStubProcessor struct {
	Events chan Event
}

func (t *testStubProcessor) ProcessFileEvent(ctx context.Context, e Event) {
	go func() {
		t.Events <- e
	}()
}

func newStubProcessor() *testStubProcessor {
	return &testStubProcessor{Events: make(chan Event, 1)}
}

func TestLogWatcher(t *testing.T) {
	t.Skip("busted ")
	testutil.SkipIfShort(t)

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)
	defer func() {
		if err = w.Close(); err != nil {
			t.Fatal(err)
		}
	}()

	s := newStubProcessor()

	if err = w.Observe(workdir, s); err != nil {
		t.Fatal(err)
	}
	f, err := os.Create(filepath.Join(workdir, "logfile"))
	testutil.FatalIfErr(t, err)
	w.Poll()
	select {
	case e := <-s.Events:
		expected := Event{Create, filepath.Join(workdir, "logfile")}
		testutil.ExpectNoDiff(t, expected, e)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("no event received before timeout")
	}

	n, err := f.WriteString("hi")
	testutil.FatalIfErr(t, err)
	if n != 2 {
		t.Fatalf("wrote %d instead of 2", n)
	}
	testutil.FatalIfErr(t, f.Close())
	w.Poll()
	select {
	case e := <-s.Events:
		expected := Event{Update, filepath.Join(workdir, "logfile")}
		testutil.ExpectNoDiff(t, expected, e)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("no event received before timeout")
	}

	testutil.FatalIfErr(t, os.Rename(filepath.Join(workdir, "logfile"), filepath.Join(workdir, "logfile2")))
	w.Poll()
	results := make([]Event, 0)
	for i := 0; i < 2; i++ {
		select {
		case e := <-s.Events:
			results = append(results, e)
		case <-time.After(100 * time.Millisecond):
			t.Fatal("no event received before timeout")
		}
	}
	expected := []Event{
		{Create, filepath.Join(workdir, "logfile2")},
		{Delete, filepath.Join(workdir, "logfile")},
	}
	sorter := func(a, b Event) bool {
		if a.Op < b.Op {
			return true
		}
		if a.Op > b.Op {
			return false
		}
		if a.Pathname < b.Pathname {
			return true
		}
		if a.Pathname > b.Pathname {
			return false
		}
		return true
	}
	testutil.ExpectNoDiff(t, expected, results, testutil.SortSlices(sorter))

	testutil.FatalIfErr(t, os.Chmod(filepath.Join(workdir, "logfile2"), os.ModePerm))
	w.Poll()
	select {
	case e := <-s.Events:
		expected := Event{Update, filepath.Join(workdir, "logfile2")}
		testutil.ExpectNoDiff(t, expected, e)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("no event received before timeout")
	}

	testutil.FatalIfErr(t, os.Remove(filepath.Join(workdir, "logfile2")))
	w.Poll()
	select {
	case e := <-s.Events:
		expected := Event{Delete, filepath.Join(workdir, "logfile2")}
		testutil.ExpectNoDiff(t, expected, e)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("no event received before timeout")
	}
}

func TestLogWatcherAddError(t *testing.T) {
	t.Skip("error injection not working")
	testutil.SkipIfShort(t)
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)
	defer func() {
		if err = w.Close(); err != nil {
			t.Fatal(err)
		}
	}()
	s := &stubProcessor{}
	filename := filepath.Join(workdir, "test")
	err = w.Observe(filename, s)
	if err == nil {
		t.Errorf("did not receive an error for nonexistent file")
	}
}

func TestLogWatcherAddWhilePermissionDenied(t *testing.T) {
	testutil.SkipIfShort(t)

	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)
	defer func() {
		if err = w.Close(); err != nil {
			t.Fatal(err)
		}
	}()

	filename := filepath.Join(workdir, "test")
	_, err = os.Create(filename)
	testutil.FatalIfErr(t, err)
	err = os.Chmod(filename, 0)
	testutil.FatalIfErr(t, err)
	s := &stubProcessor{}
	err = w.Observe(filename, s)
	if err != nil {
		t.Errorf("failed to add watch on permission denied")
	}
	err = os.Chmod(filename, 0777)
	testutil.FatalIfErr(t, err)
}

func TestWatcherNewFile(t *testing.T) {
	testutil.SkipIfShort(t)
	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()
	s := &stubProcessor{}
	testutil.FatalIfErr(t, w.Observe(tmpDir, s))
	testutil.TestOpenFile(t, path.Join(tmpDir, "log"))
	w.Poll()
	w.Close()
	expected := []Event{{Op: Create, Pathname: path.Join(tmpDir, "log")}}
	// Fetching the start of the slice is a hack because we get duplicate events.
	testutil.ExpectNoDiff(t, expected, s.Events[0:1])
}
