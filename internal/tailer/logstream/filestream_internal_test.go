// Copyright 2026 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"sync"
	"syscall"
	"testing"

	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

// staleReader returns ESTALE on every read, counting them, until `limit` reads
// have been made, after which it returns EOF.  The limit stops a stream that
// reopens without waiting from spinning until the test deadline.  Every read
// announces itself on `entered` and then waits for `release` to be closed, so
// a test can act while the stream is blocked in a read.
type staleReader struct {
	limit   int
	entered chan struct{}
	release chan struct{}

	mu    sync.Mutex
	count int
}

func (r *staleReader) Read(_ []byte) (int, error) {
	if r.entered != nil {
		r.entered <- struct{}{}
	}
	if r.release != nil {
		<-r.release
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	r.count++
	if r.count > r.limit {
		return 0, io.EOF
	}
	return 0, &os.PathError{Op: "read", Path: "log", Err: syscall.ESTALE}
}

func (r *staleReader) reads() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.count
}

// injectReader makes every file stream read from `r` instead of the file it
// opened, for the duration of the test.
func injectReader(t *testing.T, r io.Reader) {
	t.Helper()
	saved := newLogFileReader
	newLogFileReader = func(_ *os.File) io.Reader { return r }
	t.Cleanup(func() { newLogFileReader = saved })
}

// testLogFile creates an empty log to stream from, returning its pathname.
func testLogFile(t *testing.T) string {
	t.Helper()
	name := filepath.Join(testutil.TestTempDir(t), "log")
	f := testutil.TestOpenFile(t, name)
	testutil.FatalIfErr(t, f.Close())
	return name
}

func TestFileStreamStaleWaitsBetweenReopens(t *testing.T) {
	var wg sync.WaitGroup

	r := &staleReader{limit: 100}
	injectReader(t, r)
	name := testLogFile(t)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := New(ctx, &wg, w, name, OneShotDisabled)
	testutil.FatalIfErr(t, err)
	go func() {
		for range fs.Lines() {
		}
	}()

	// The first read happens before the stream waits, so there is one more
	// read than there are wakeups.
	for i := 0; i < 3; i++ {
		awaken(1, 1)
	}

	if got, want := r.reads(), 4; got != want {
		t.Errorf("stale reads: got %d, want %d (the stream is reopening without waiting)", got, want)
	}

	cancel()
	wg.Wait()
}

func TestFileStreamStaleReopenFailureEndsStream(t *testing.T) {
	var wg sync.WaitGroup

	r := &staleReader{limit: 100, entered: make(chan struct{}), release: make(chan struct{})}
	injectReader(t, r)
	name := testLogFile(t)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w, awaken := waker.NewTest(ctx, 1, "stream")

	fs, err := New(ctx, &wg, w, name, OneShotDisabled)
	testutil.FatalIfErr(t, err)

	// Take the log away while the stream is blocked in its first read, so
	// that the reopen after the stale read is certain to fail.
	<-r.entered
	testutil.FatalIfErr(t, os.Remove(name))
	close(r.release)

	go awaken(1, 0)
	wg.Wait()

	select {
	case line, ok := <-fs.Lines():
		if ok {
			t.Errorf("expecting the stream to be complete because the reopen failed, got %v", line)
		}
	default:
		t.Error("expecting the lines channel to be closed after a failed reopen")
	}
}
