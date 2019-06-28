// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"context"
	"expvar"
	"fmt"
	"os"
	"os/user"
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

// This test requires disk access, and cannot be injected without internal
// knowledge of the fsnotify code. Make the wait deadlines long.
const deadline = 5 * time.Second

type testStubProcessor struct {
	Events []Event
}

func (t *testStubProcessor) ProcessFileEvent(ctx context.Context, e Event) {
	t.Events = append(t.Events, e)
}

func TestLogWatcher(t *testing.T) {
	if testing.Short() {
		// This test is slow due to disk access.
		t.Skip("skipping log watcher test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0, true)
	if err != nil {
		t.Fatalf("couldn't create a watcher: %s\n", err)
	}
	defer func() {
		if err = w.Close(); err != nil {
			t.Fatal(err)
		}
	}()

	s := &stubProcessor{}

	if err = w.Observe(workdir, s); err != nil {
		t.Fatal(err)
	}
	f, err := os.Create(filepath.Join(workdir, "logfile"))
	testutil.FatalIfErr(t, err)
	check := func(count int) func() (bool, error) {
		return func() (bool, error) {
			if len(s.Events) == count {
				return true, nil
			}
			return false, nil
		}
	}
	ok, err := testutil.DoOrTimeout(check(1), 100*time.Millisecond, 10*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if len(s.Events) == 0 {
		t.Errorf("no event received")
	}
	if s.Events[0].Op != Create {
		t.Errorf("wrong event type: %q", s.Events[0])
	}
	if s.Events[0].Pathname != filepath.Join(workdir, "logfile") {
		t.Errorf("pathname doesn't match: %q", s.Events[0])
	}

	n, err := f.WriteString("hi")
	testutil.FatalIfErr(t, err)
	if n != 2 {
		t.Fatalf("wrote %d instead of 2", n)
	}
	testutil.FatalIfErr(t, f.Close())
	ok, err = testutil.DoOrTimeout(check(2), 100*time.Millisecond, 10*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if !ok {
		t.Errorf("no event received")
	}
	if s.Events[1].Op != Update {
		t.Errorf("wrong event type: %q", s.Events[1])
	}
	if s.Events[1].Pathname != filepath.Join(workdir, "logfile") {
		t.Errorf("pathname doesn't match: %q", s.Events[1])
	}
	testutil.FatalIfErr(t, os.Rename(filepath.Join(workdir, "logfile"), filepath.Join(workdir, "logfile2")))
	ok, err = testutil.DoOrTimeout(check(4), 100*time.Millisecond, 10*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if !ok {
		t.Errorf("no events received")
	}
	if s.Events[2].Op != Delete {
		t.Errorf("wrong event type: %q", s.Events[2])
	}
	if s.Events[2].Pathname != filepath.Join(workdir, "logfile") {
		t.Errorf("pathname doesn't match: %q", s.Events[2])
	}
	if s.Events[3].Op != Create {
		t.Errorf("wrong event type: %q", s.Events[3])
	}
	if s.Events[3].Pathname != filepath.Join(workdir, "logfile2") {
		t.Errorf("pathname doesn't match: %q", s.Events[3])
	}

	testutil.FatalIfErr(t, os.Chmod(filepath.Join(workdir, "logfile2"), os.ModePerm))
	ok, err = testutil.DoOrTimeout(check(5), 100*time.Millisecond, 10*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if !ok {
		t.Errorf("no event recieved")
	}
	if s.Events[4].Op != Update {
		t.Errorf("wrong event type: %q", s.Events[4])
	}
	if s.Events[4].Pathname != filepath.Join(workdir, "logfile2") {
		t.Errorf("pathname doesn't match: %q", s.Events[4])
	}

	testutil.FatalIfErr(t, os.Remove(filepath.Join(workdir, "logfile2")))
	ok, err = testutil.DoOrTimeout(check(6), 100*time.Millisecond, 10*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if !ok {
		t.Errorf("no event received")
	}
	if s.Events[5].Op != Delete {
		t.Errorf("wrong event type: %q", s.Events[5])
	}
	if s.Events[5].Pathname != filepath.Join(workdir, "logfile2") {
		t.Errorf("pathname doesn't match: %q", s.Events[5])
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
	if err := syscall.Getrlimit(syscall.RLIMIT_NOFILE, &rLimit); err != nil {
		t.Fatalf("couldn't get rlimit: %s", err)
	}
	var zero = rLimit
	zero.Cur = 0
	if err := syscall.Setrlimit(syscall.RLIMIT_NOFILE, &zero); err != nil {
		t.Fatalf("couldn't set rlimit: %s", err)
	}
	_, err := NewLogWatcher(0, true)
	if err != nil {
		t.Error(err)
	}
	if err := syscall.Setrlimit(syscall.RLIMIT_NOFILE, &rLimit); err != nil {
		t.Fatalf("couldn't reset rlimit: %s", err)
	}
}

func TestLogWatcherAddError(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0, true)
	if err != nil {
		t.Fatalf("couldn't create a watcher: %s\n", err)
	}
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
	u, err := user.Current()
	if err != nil {
		t.Skip(fmt.Sprintf("Couldn't determine current user id: %s", err))
	}
	if u.Uid == "0" {
		t.Skip("Skipping test when run as root")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	w, err := NewLogWatcher(0, true)
	if err != nil {
		t.Fatalf("couldn't create a watcher: %s\n", err)
	}
	defer func() {
		if err = w.Close(); err != nil {
			t.Fatal(err)
		}
	}()

	filename := filepath.Join(workdir, "test")
	if _, err = os.Create(filename); err != nil {
		t.Fatalf("couldn't create file: %s", err)
	}
	if err = os.Chmod(filename, 0); err != nil {
		t.Fatalf("couldn't chmod file: %s", err)
	}
	s := &stubProcessor{}
	err = w.Observe(filename, s)
	if err != nil {
		t.Errorf("failed to add watch on permission denied")
	}
	if err := os.Chmod(filename, 0777); err != nil {
		t.Fatalf("couldn't reset file perms: %s", err)
	}
}

func TestWatcherErrors(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}
	orig, err := strconv.ParseInt(expvar.Get("log_watcher_error_count").String(), 10, 64)
	if err != nil {
		t.Fatalf("couldn't convert expvar %q", expvar.Get("log_watcher_error_count").String())
	}
	w, err := NewLogWatcher(0, true)
	if err != nil {
		t.Fatalf("couldn't create a watcher")
	}
	w.watcher.Errors <- errors.New("Injected error for test")
	if err := w.Close(); err != nil {
		t.Fatalf("watcher close failed: %q", err)
	}
	expected := strconv.FormatInt(orig+1, 10)
	if diff := testutil.Diff(expected, expvar.Get("log_watcher_error_count").String()); diff != "" {
		t.Errorf("log watcher error count not increased:\n%s", diff)
	}
}

func TestWatcherNewFile(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}
	tests := []struct {
		d time.Duration
		b bool
	}{
		{0, true},
		{10 * time.Millisecond, false},
	}
	for _, test := range tests {
		t.Run(fmt.Sprintf("%s %v", test.d, test.b), func(t *testing.T) {
			w, err := NewLogWatcher(test.d, test.b)
			testutil.FatalIfErr(t, err)
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()
			s := &stubProcessor{}
			testutil.FatalIfErr(t, w.Observe(tmpDir, s))
			testutil.TestOpenFile(t, path.Join(tmpDir, "log"))
			time.Sleep(250 * time.Millisecond)
			w.Close()
			expected := []Event{{Op: Create, Pathname: path.Join(tmpDir, "log")}}
			if diff := testutil.Diff(expected, s.Events); diff != "" {
				t.Errorf("event unexpected: diff:\n%s", diff)
				t.Logf("received:\n%v", s.Events)
			}
		})
	}
}
