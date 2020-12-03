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
)

type pipeStream struct {
	ctx          context.Context
	pathname     string    // Given name for the underlying named pipe on the filesystem
	lastReadTime time.Time // Last time a log line was read from this named pipe
	file         *os.File  // The file descriptor of the open pipe
	llp          logline.Processor
	wakeChannel  chan struct{}

	finishedMu sync.Mutex // protects `finished`
	finished   bool       // This pipestream is finished and can no longer be used.
}

func newPipeStream(ctx context.Context, wg *sync.WaitGroup, pathname string, fi os.FileInfo, llp logline.Processor) (LogStream, error) {
	ps := &pipeStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp, wakeChannel: make(chan struct{}, 1)}
	wg.Add(1)
	go ps.read(ctx, wg, fi)
	return ps, nil
}

func (ps *pipeStream) LastReadTime() time.Time {
	return ps.lastReadTime
}

func (ps *pipeStream) Wake() {
	ps.wakeChannel <- struct{}{}
}

func (ps *pipeStream) read(ctx context.Context, wg *sync.WaitGroup, fi os.FileInfo) {
	defer wg.Done()
	// Open in nonblocking mode because the write end of the pipe may not have started yet.
	fd, err := os.OpenFile(ps.pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0600)
	if err != nil {
		glog.Info(err)
		return
	}
	defer func() {
		err := fd.Close()
		if err != nil {
			glog.Info(err)
		}
	}()
	b := make([]byte, 0, defaultReadBufferSize)
	capB := cap(b)
	partial := bytes.NewBufferString("")
	for {
		// Set idle timeout
		if err := fd.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
			glog.V(2).Infof("%s: %s", ps.pathname, err)
		}
		n, err := fd.Read(b[:capB])
		if e, ok := err.(*os.PathError); ok && e.Timeout() && n == 0 {
			// Named Pipes EOF when the writer has closed, so we look for a
			// timeout on read to detect a writer stall and thus let us check
			// below for cancellation.
			goto Sleep
		}
		if err == io.EOF {
			// Per pipe(7): If all file descriptors referring to the write end
			// of a pipe have been closed, then an attempt to read(2) from the
			// pipe will see end-of-file (read(2) will return 0).
			ps.finishedMu.Lock()
			ps.finished = true
			ps.finishedMu.Unlock()
			return
		}

		decodeAndSend(ps.ctx, ps.llp, ps.pathname, n, b[:n], partial)
		// Update the last read time if we were able to read anything.
		if n > 0 {
			ps.lastReadTime = time.Now()
		}
	Sleep:
		select {
		case <-ps.wakeChannel:
			// sleep until next Wake()
		case <-ctx.Done():
			ps.finishedMu.Lock()
			ps.finished = true
			ps.finishedMu.Unlock()
			return
		}
	}
}

func (ps *pipeStream) IsFinished() bool {
	ps.finishedMu.Lock()
	defer ps.finishedMu.Unlock()
	return ps.finished
}
