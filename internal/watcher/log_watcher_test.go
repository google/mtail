// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"context"
	"expvar"
	"fmt"
	"os"
	"path"
	"path/filepath"
	"strconv"
	"syscall"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/testutil"
	"github.com/pkg/errors"
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
	if testing.Short() {
		// This test is slow due to disk access.
		t.Skip("skipping log watcher test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0, true)
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
	select {
	case e := <-s.Events:
		expected := Event{Update, filepath.Join(workdir, "logfile")}
		testutil.ExpectNoDiff(t, expected, e)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("no event received before timeout")
	}

	testutil.FatalIfErr(t, os.Rename(filepath.Join(workdir, "logfile"), filepath.Join(workdir, "logfile2")))
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
	select {
	case e := <-s.Events:
		expected := Event{Update, filepath.Join(workdir, "logfile2")}
		testutil.ExpectNoDiff(t, expected, e)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("no event received before timeout")
	}

	testutil.FatalIfErr(t, os.Remove(filepath.Join(workdir, "logfile2")))
	select {
	case e := <-s.Events:
		expected := Event{Delete, filepath.Join(workdir, "logfile2")}
		testutil.ExpectNoDiff(t, expected, e)
	case <-time.After(100 * time.Millisecond):
		t.Fatal("no event received before timeout")
	}
}

// This test may be OS specific; possibly break it out to a file with build tags.
func TestFsnotifyErrorFallbackToPoll(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}
	// The Warning log isn't created until the first write.  Create it before
	// setting the rlimit on open files or the test will fail trying to open
	// the log file instead of where it should.
	glog.Warning("pre-creating log to avoid too many open file")

	var rLimit syscall.Rlimit
	err := syscall.Getrlimit(syscall.RLIMIT_NOFILE, &rLimit)
	testutil.FatalIfErr(t, err)
	var zero = rLimit
	zero.Cur = 0
	err = syscall.Setrlimit(syscall.RLIMIT_NOFILE, &zero)
	testutil.FatalIfErr(t, err)
	_, err = NewLogWatcher(0, true)
	if err != nil {
		t.Error(err)
	}
	err = syscall.Setrlimit(syscall.RLIMIT_NOFILE, &rLimit)
	testutil.FatalIfErr(t, err)
}

func TestLogWatcherAddError(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0, true)
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
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}

	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0, true)
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

func TestWatcherErrors(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}
	orig, err := strconv.ParseInt(expvar.Get("log_watcher_errors_total").String(), 10, 64)
	if err != nil {
		t.Fatalf("couldn't convert expvar %q", expvar.Get("log_watcher_errors_total").String())
	}
	w, err := NewLogWatcher(0, true)
	testutil.FatalIfErr(t, err)
	w.watcher.Errors <- errors.New("Injected error for test")
	err = w.Close()
	testutil.FatalIfErr(t, err)
	expected := strconv.FormatInt(orig+1, 10)
	testutil.ExpectNoDiff(t, expected, expvar.Get("log_watcher_errors_total").String())
}

func TestWatcherNewFile(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}
	tests := []struct {
		period   time.Duration
		fsnotify bool
	}{
		{0, true},
		{10 * time.Millisecond, false},
	}
	for _, test := range tests {
		t.Run(fmt.Sprintf("%s %v", test.period, test.fsnotify), func(t *testing.T) {
			w, err := NewLogWatcher(test.period, test.fsnotify)
			testutil.FatalIfErr(t, err)
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()
			s := &stubProcessor{}
			testutil.FatalIfErr(t, w.Observe(tmpDir, s))
			testutil.TestOpenFile(t, path.Join(tmpDir, "log"))
			if !test.fsnotify {
				w.Poll()
			} else {
				// wait a bit for kernel to notice
				time.Sleep(250 * time.Millisecond)
			}
			w.Close()
			expected := []Event{{Op: Create, Pathname: path.Join(tmpDir, "log")}}
			// Fetching the start of the slice is a hack because we get duplicate events.
			testutil.ExpectNoDiff(t, expected, s.Events[0:1])
		})
	}
}
