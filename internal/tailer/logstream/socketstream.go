// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"net"
	"os"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
)

type socketStream struct {
	ctx          context.Context
	pathname     string        // Given name for the underlying socket path on the filesystem
	lastReadTime time.Time     // Last time a log line was read from this socket
	c            net.Conn      // Connection for the open socket
	partial      *bytes.Buffer // Partial line accumulator
	llp          logline.Processor
	pollInterval time.Duration
}

func newSocketStream(ctx context.Context, wg *sync.WaitGroup, pathname string, fi os.FileInfo, llp logline.Processor, pollInterval time.Duration) (LogStream, error) {
	ss := &socketStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp, pollInterval: pollInterval}
	wg.Add(1)
	go ss.read(ctx, wg, fi)
	return ss, nil
}

func (ss *socketStream) LastReadTime() time.Time {
	return ss.lastReadTime
}

func (ss *socketStream) read(ctx context.Context, wg sync.WaitGroup, fi os.FileInfo) {
	defer wg.Done()
	c, err := net.ListenUnixgram("unixgram", &net.UnixAddr{pathname, "unixgram"})
	if err != nil {
		glog.Info(err)
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
		if err := ss.c.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
			glog.V(2).Infof("%s: %s", ss.pathname, err)
		}

		n, err := ss.c.Read(b[:capB])
		if err, ok := err.(net.Error); ok && err.Timeout() {
			// Timeout, return
			goto Sleep
		}

		totalBytes += n
		b = b[:n]

		decodeAndSend(ss.ctx, ss.llp, ss.pathname, n, &b, ss.partial)

		if totalBytes > 0 {
			// Don't bother updating lastRead until we return. // TODO: bug?
			ss.lastReadTime = time.Now()
		}
	Sleep:
		select {
		case <-time.After(ss.pollInterval):
			// sleep to next read
		case <-ctx.Done():
			return
		}
	}
}
