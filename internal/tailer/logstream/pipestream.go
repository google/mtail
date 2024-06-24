// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"os"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

type pipeStream struct {
	lines chan *logline.LogLine

	pathname string // Given name for the underlying named pipe on the filesystem

	mu           sync.RWMutex // protects following fields
	completed    bool         // This pipestream is completed and can no longer be used.
	lastReadTime time.Time    // Last time a log line was read from this named pipe
}

// newPipeStream creates a new stream reader for Unix Pipes.
// `pathname` must already be verified as clean.
func newPipeStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo) (LogStream, error) {
	ps := &pipeStream{pathname: pathname, lastReadTime: time.Now(), lines: make(chan *logline.LogLine)}
	if err := ps.stream(ctx, wg, waker, fi); err != nil {
		return nil, err
	}
	return ps, nil
}

func (ps *pipeStream) LastReadTime() time.Time {
	ps.mu.RLock()
	defer ps.mu.RUnlock()
	return ps.lastReadTime
}

func pipeOpen(pathname string) (*os.File, error) {
	if IsStdinPattern(pathname) {
		return os.Stdin, nil
	}
	// Open in nonblocking mode because the write end of the pipe may not have started yet.
	return os.OpenFile(pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0o600) // #nosec G304 -- path already validated by caller
}

// The read buffer size for pipes.
//
// Before Linux 2.6.11, the capacity of a pipe was the same as the
// system page size (e.g., 4096 bytes on i386).  Since Linux 2.6.11,
// the pipe capacity is 16 pages (i.e., 65,536 bytes in a system
// with a page size of 4096 bytes).  Since Linux 2.6.35, the default
// pipe capacity is 16 pages, but the capacity can be queried and
// set using the fcntl(2) F_GETPIPE_SZ and F_SETPIPE_SZ operations.
// See fcntl(2) for more information.
//
// https://man7.org/linux/man-pages/man7/pipe.7.html
const defaultPipeReadBufferSize = 131072

func (ps *pipeStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, _ os.FileInfo) error {
	fd, err := pipeOpen(ps.pathname)
	if err != nil {
		logErrors.Add(ps.pathname, 1)
		return err
	}
	glog.V(2).Infof("stream(%s): opened new pipe %v", ps.pathname, fd)
	b := make([]byte, defaultPipeReadBufferSize)
	partial := bytes.NewBufferString("")
	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("stream(%s): read total %d bytes", ps.pathname, total)
			glog.V(2).Infof("stream(%s): closing file descriptor %v", ps.pathname, fd)
			err := fd.Close()
			if err != nil {
				logErrors.Add(ps.pathname, 1)
				glog.Info(err)
			}
			logCloses.Add(ps.pathname, 1)
			ps.mu.Lock()
			ps.completed = true
			close(ps.lines)
			ps.mu.Unlock()
		}()
		ctx, cancel := context.WithCancel(ctx)
		defer cancel()
		SetReadDeadlineOnDone(ctx, fd)

		for {
			n, err := fd.Read(b)
			glog.V(2).Infof("stream(%s): read %d bytes, err is %v", ps.pathname, n, err)

			if n > 0 {
				total += n
				decodeAndSend(ctx, ps.lines, ps.pathname, n, b[:n], partial)
				// Update the last read time if we were able to read anything.
				ps.mu.Lock()
				ps.lastReadTime = time.Now()
				ps.mu.Unlock()
			}

			// Test to see if we should exit.
			if err != nil && IsEndOrCancel(err) {
				if partial.Len() > 0 {
					sendLine(ctx, ps.pathname, partial, ps.lines)
				}
				glog.V(2).Infof("stream(%s): exiting, stream has error %s", ps.pathname, err)
				return
			}

			// Wait for wakeup or termination.
			glog.V(2).Infof("stream(%s): waiting", ps.pathname)
			select {
			case <-ctx.Done():
				// Exit immediately; cancelled context is going to cause the
				// next read to be interrupted and exit, so don't bother going
				// around the loop again.
				return
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("stream(%s): Wake received", ps.pathname)
			}
		}
	}()
	return nil
}

func (ps *pipeStream) IsComplete() bool {
	ps.mu.RLock()
	defer ps.mu.RUnlock()
	return ps.completed
}

// Stop implements the Logstream interface.
// Calling Stop on a PipeStream is a no-op; PipeStreams always read until the input pipe is closed, which is what calling Stop means on a Logstream.
func (ps *pipeStream) Stop() {
}

// Lines implements the LogStream interface, returning the output lines channel.
func (ps *pipeStream) Lines() <-chan *logline.LogLine {
	return ps.lines
}
