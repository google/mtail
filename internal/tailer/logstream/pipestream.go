// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"errors"
	"io"
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
	fd, err := os.OpenFile(ps.pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0600)
	if err != nil {
		logErrors.Add(ps.pathname, 1)
		return err
	}
	glog.V(2).Infof("opened new pipe %v", fd)
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
		b := make([]byte, 0, defaultReadBufferSize)
		capB := cap(b)
		partial := bytes.NewBufferString("")
		var timedout bool
		for {
			// Set idle timeout
			if err := fd.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
				logErrors.Add(ps.pathname, 1)
				glog.V(2).Infof("%s: %s", ps.pathname, err)
			}
			n, err := fd.Read(b[:capB])
			glog.V(2).Infof("%v: read %d bytes, err is %v", fd, n, err)

			if n > 0 {
				total += n
				decodeAndSend(ps.ctx, ps.lines, ps.pathname, n, b[:n], partial)
				// Update the last read time if we were able to read anything.
				ps.mu.Lock()
				ps.lastReadTime = time.Now()
				ps.mu.Unlock()
			}

			var perr *os.PathError
			if errors.As(err, &perr) && perr.Timeout() && n == 0 {
				glog.V(2).Info("timed out")
				timedout = true
				// Named Pipes EOF only when the writer has closed, so we look
				// for a timeout on read to detect a writer stall and thus let
				// us check below for cancellation.
				goto Sleep
			}
			// Per pipe(7): If all file descriptors referring to the write end
			// of a pipe have been closed, then an attempt to read(2) from the
			// pipe will see end-of-file (read(2) will return 0).
			// All other errors also finish the stream and are counted.
			// However when the pipe is freshly opened
			if err != nil {
				if err != io.EOF {
					glog.Info(err)
					logErrors.Add(ps.pathname, 1)
				}
				glog.V(2).Infof("%v: stream has errored, exiting", fd)
				ps.mu.Lock()
				ps.completed = true
				ps.mu.Unlock()
				return
			}

			// No error implies there's more to read, unless it looks like
			// context is Done.
			if err == nil && ctx.Err() == nil {
				continue
			}

		Sleep:
			// If we've stalled or it looks like the context is done, then test to see if it's time to exit.
			if timedout || ctx.Err() != nil {
				timedout = false
				// Test to see if it's time to exit.
				select {
				case <-ctx.Done():
					glog.V(2).Infof("%v: context has been cancelled, exiting", fd)
					if partial.Len() > 0 {
						sendLine(ctx, ps.pathname, partial, ps.lines)
					}
					ps.mu.Lock()
					ps.completed = true
					ps.mu.Unlock()
					return
				default:
					// keep going
				}
			}
			// Yield and wait
			glog.V(2).Infof("%v: waiting", fd)
			select {
			case <-ctx.Done():
				// Same for cancellation; this makes tests stable, but
				// could argue exiting immediately is less surprising.
				// Assumption is that this doesn't make a difference in
				// production.
				glog.V(2).Infof("%v: Cancelled after next read", fd)
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
// Calling Stop on a pipe is a no-op; Pipes always read until the pipe is closed, which is what calling Stop means on a Logstream.
func (ps *pipeStream) Stop() {
}
