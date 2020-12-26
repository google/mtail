// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"context"
	"os"
	"path"
	"path/filepath"
	"testing"

	"github.com/google/mtail/internal/testutil"
)

type testStubProcessor struct {
	Events []Event
}

func (t *testStubProcessor) ProcessFileEvent(ctx context.Context, e Event) {
	t.Events = append(t.Events, e)
}

func newStubProcessor() *testStubProcessor {
	return &testStubProcessor{Events: make([]Event, 0)}
}

func TestLogWatcher(t *testing.T) {
	t.Skip("flaky")
	testutil.SkipIfShort(t)

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)
	defer func() {
		testutil.FatalIfErr(t, w.Close())
	}()

	s := newStubProcessor()
	expected := []Event{}

	testutil.FatalIfErr(t, w.Observe(workdir, s))

	f := testutil.TestOpenFile(t, filepath.Join(workdir, "logfile"))
	w.Poll()
	expected = append(expected, Event{Create, filepath.Join(workdir, "logfile")})
	testutil.ExpectNoDiff(t, expected, s.Events)
	// Simulate the processor observing the new log
	testutil.FatalIfErr(t, w.Observe(filepath.Join(workdir, "logfile"), s))

	testutil.WriteString(t, f, "hi")
	testutil.FatalIfErr(t, f.Close())
	w.Poll()
	expected = append(expected, Event{Update, filepath.Join(workdir, "logfile")})
	testutil.ExpectNoDiff(t, expected, s.Events)

	testutil.FatalIfErr(t, os.Rename(filepath.Join(workdir, "logfile"), filepath.Join(workdir, "logfile2")))
	w.Poll()
	expected = append(expected, Event{Create, filepath.Join(workdir, "logfile2")},
		Event{Delete, filepath.Join(workdir, "logfile")})
	testutil.ExpectNoDiff(t, expected, s.Events)
	// Simulate watch on logfile2
	testutil.FatalIfErr(t, w.Observe(filepath.Join(workdir, "logfile2"), s))

	testutil.FatalIfErr(t, os.Chmod(filepath.Join(workdir, "logfile2"), os.ModePerm))
	w.Poll()
	expected = append(expected, Event{Update, filepath.Join(workdir, "logfile2")})
	testutil.ExpectNoDiff(t, expected, s.Events)

	testutil.FatalIfErr(t, os.Remove(filepath.Join(workdir, "logfile2")))
	w.Poll()
	expected = append(expected, Event{Delete, filepath.Join(workdir, "logfile2")})
	testutil.ExpectNoDiff(t, expected, s.Events)
}

func TestLogWatcherAddNotFound(t *testing.T) {
	testutil.SkipIfShort(t)
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)
	defer func() {
		testutil.FatalIfErr(t, w.Close())
	}()
	s := &stubProcessor{}
	filename := filepath.Join(workdir, "test")
	err = w.Observe(filename, s)
	if err == nil {
		t.Errorf("did not receive an error for nonexistent file")
	}
}

func TestLogWatcherAddPermissionDenied(t *testing.T) {
	testutil.SkipIfShort(t)

	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)
	defer func() {
		testutil.FatalIfErr(t, w.Close())
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

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	w, err := NewLogWatcher(0)
	testutil.FatalIfErr(t, err)

	s := &stubProcessor{}
	testutil.FatalIfErr(t, w.Observe(tmpDir, s))
	testutil.TestOpenFile(t, path.Join(tmpDir, "log"))
	w.Poll()
	w.Close()
	expected := []Event{{Op: Create, Pathname: path.Join(tmpDir, "log")}}
	testutil.ExpectNoDiff(t, expected, s.Events)
}
