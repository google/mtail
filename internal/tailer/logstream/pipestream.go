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
)

type pipeStream struct {
	ctx          context.Context
	pathname     string        // Given name for the underlying named pipe on the filesystem
	lastReadTime time.Time     // Last time a log line was read from this named pipe
	file         *os.File      // The file descriptor of the open pipe
	partial      *bytes.Buffer // Partial line accumulator
	llp          logline.Processor
	pollInterval time.Duration
}

// TODO: if the pipe is closed by the writer while mtail is running, and then reopened, what happens? Bug report says we should exit if the pipe is os.Stdin and we have no other logs being watched.
func newPipeStream(ctx context.Context, wg *sync.WaitGroup, pathname string, fi os.FileInfo, llp logline.Processor, pollInterval time.Duration) (LogStream, error) {
	ps := &pipeStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp, pollInterval: pollInterval}
	wg.Add(1)
	go ps.read(ctx, wg, fi)
	return ps, nil
}

func (ps *pipeStream) LastReadTime() time.Time {
	return ps.lastReadTime
}

func (ps *pipeStream) read(ctx context.Context, wg *sync.WaitGroup, fi os.FileInfo) {
	defer wg.Done()
	// Open in nonblocking mode because the write end of the pipe may not have started yet.
	fd, err := os.OpenFile(pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0600)
	if err != nil {
		return nil, err
	}
	defer func() {
		err := fd.Close()
		if err != nil {
			glog.Info(err)
		}
	}()
	b := make([]byte, 0, defaultReadBufferSize)
	capB := cap(b)
	totalBytes := 0
	partial := bytes.NewBufferString("")
	for {
		// Set idle timeout
		if err := fd.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
			glog.V(2).Infof("%s: %s", ps.pathname, err)
		}
		n, err := fd.Read(b[:capB])
		if e, ok := err.(*os.PathError); ok && e.Timeout() && n == 0 {
			// Named Pipes have no EOF so we look for a timeout on read to
			// detect an EOF-like condition
			goto Sleep
		}

		totalBytes += n
		b = b[:n]

		decodeAndSend(ps.ctx, ps.llp, ps.pathname, n, &b, ps.partial)
		// Update the last read time if we were able to read anything.
		if totalBytes > 0 {
			ps.lastReadTime = time.Now()
		}
	Sleep:
		select {
		case <-time.After(fs.pollInterval):
			// sleep to next read
		case <-ctx.Done():
			return
		}
	}
}
