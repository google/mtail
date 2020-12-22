// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
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
	ctx          context.Context
	pathname     string    // Given name for the underlying named pipe on the filesystem
	lastReadTime time.Time // Last time a log line was read from this named pipe
	llp          logline.Processor

	finishedMu sync.Mutex // protects `finished`
	finished   bool       // This pipestream is finished and can no longer be used.
}

func newPipeStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo, llp logline.Processor) (LogStream, error) {
	ps := &pipeStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp}
	if err := ps.stream(ctx, wg, waker, fi); err != nil {
		return nil, err
	}
	return ps, nil
}

func (ps *pipeStream) LastReadTime() time.Time {
	return ps.lastReadTime
}

func (ps *pipeStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, fi os.FileInfo) error {
	// Open in nonblocking mode because the write end of the pipe may not have started yet.
	fd, err := os.OpenFile(ps.pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0600)
	if err != nil {
		logErrors.Add(ps.pathname, 1)
		return err
	}
	go func() {
		wg.Add(1)
		defer wg.Done()
		defer func() {
			err := fd.Close()
			if err != nil {
				logErrors.Add(ps.pathname, 1)
				glog.Info(err)
			}
		}()
		b := make([]byte, 0, defaultReadBufferSize)
		capB := cap(b)
		partial := bytes.NewBufferString("")
		for {
			// Set idle timeout
			if err := fd.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
				logErrors.Add(ps.pathname, 1)
				glog.V(2).Infof("%s: %s", ps.pathname, err)
			}
			n, err := fd.Read(b[:capB])
			if e, ok := err.(*os.PathError); ok && e.Timeout() && n == 0 {
				// Named Pipes EOF when the writer has closed, so we look for a
				// timeout on read to detect a writer stall and thus let us check
				// below for cancellation.
				goto Sleep
			}
			// Per pipe(7): If all file descriptors referring to the write end
			// of a pipe have been closed, then an attempt to read(2) from the
			// pipe will see end-of-file (read(2) will return 0).
			// All other errors also finish the stream and are counted.
			if err != nil {
				if err != io.EOF {
					glog.Info(err)
					logErrors.Add(ps.pathname, 1)
				}
				ps.finishedMu.Lock()
				ps.finished = true
				ps.finishedMu.Unlock()
				return
			}

			decodeAndSend(ps.ctx, ps.llp, ps.pathname, n, b[:n], partial)
			if n > 0 {
				ps.lastReadTime = time.Now()
			}
		Sleep:
			select {
			case <-ctx.Done():
				ps.finishedMu.Lock()
				ps.finished = true
				ps.finishedMu.Unlock()
				return
			case <-waker.Wake():
				// sleep until next Wake()
			}
		}
	}()
	return nil
}

func (ps *pipeStream) IsFinished() bool {
	ps.finishedMu.Lock()
	defer ps.finishedMu.Unlock()
	return ps.finished
}
