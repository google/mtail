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
	ctx   context.Context
	lines chan<- *logline.LogLine

	pathname string // Given name for the underlying named pipe on the filesystem

	mu           sync.RWMutex // protects following fields
	completed    bool         // This pipestream is completed and can no longer be used.
	lastReadTime time.Time    // Last time a log line was read from this named pipe
}

func newPipeStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo, lines chan<- *logline.LogLine) (LogStream, error) {
	ps := &pipeStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), lines: lines}
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

func (ps *pipeStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, fi os.FileInfo) error {
	// Open in nonblocking mode because the write end of the pipe may not have started yet.
	fd, err := os.OpenFile(ps.pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0o600)
	if err != nil {
		logErrors.Add(ps.pathname, 1)
		return err
	}
	glog.V(2).Infof("opened new pipe %v", fd)
	b := make([]byte, defaultReadBufferSize)
	partial := bytes.NewBufferString("")
	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("%v: read total %d bytes from %s", fd, total, ps.pathname)
			glog.V(2).Infof("%v: closing file descriptor", fd)
			err := fd.Close()
			if err != nil {
				logErrors.Add(ps.pathname, 1)
				glog.Info(err)
			}
			logCloses.Add(ps.pathname, 1)
			ps.mu.Lock()
			ps.completed = true
			ps.mu.Unlock()
		}()
		ctx, cancel := context.WithCancel(ctx)
		defer cancel()
		SetReadDeadlineOnDone(ctx, fd)

		for {
			n, err := fd.Read(b)
			glog.V(2).Infof("%v: read %d bytes, err is %v", fd, n, err)

			if n > 0 {
				total += n
				decodeAndSend(ps.ctx, ps.lines, ps.pathname, n, b[:n], partial)
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
				glog.V(2).Infof("%v: exiting, stream has error %s", fd, err)
				return
			}

			// Wait for wakeup or termination.
			glog.V(2).Infof("%v: waiting", fd)
			select {
			case <-ctx.Done():
				// Exit immediately; cancelled context is going to cause the
				// next read to be interrupted and exit, so don't bother going
				// around the loop again.
				return
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("%v: Wake received", fd)
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
// Calling Stop on a PipeStream is a no-op; PipeStreams always read until the pipe is closed, which is what calling Stop means on a Logstream.
func (ps *pipeStream) Stop() {
}
