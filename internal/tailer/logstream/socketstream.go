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
	pathname     string        // Given name for the underlying named pipe on the filesystem
	lastReadTime time.Time     // Last time a log line was read from this named pipe
	c            net.Conn      // Connection for the open socket
	partial      *bytes.Buffer // Partial line accumulator
	llp          logline.Processor
}

func newSocketStream(ctx context.Context, wg *sync.WaitGroup, pathname string, llp logline.Processor) (LogStream, error) {
	c, err := net.ListenUnixgram("unixgram", &net.UnixAddr{pathname, "unixgram"})
	if err != nil {
		return nil, err
	}
	ss := &socketStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), c: c, partial: bytes.NewBufferString(""), llp: llp}
	return ss, nil
}

func (ss *socketStream) Reopen(fi *os.FileInfo) error {
	return nil
}

func (ss *socketStream) Close() error {
	return nil
}

func (ss *socketStream) LastReadTime() time.Time {
	return ss.lastReadTime
}

func (ss *socketStream) read() error {
	b := make([]byte, 0, defaultReadBufferSize)
	capB := cap(b)
	totalBytes := 0
	for {
		if err := ss.c.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
			glog.V(2).Infof("%s: %s", ss.pathname, err)
		}

		n, err := ss.c.Read(b[:capB])
		if err, ok := err.(net.Error); ok && err.Timeout() {
			// Timeout, return
			return nil
		}

		totalBytes += n
		b = b[:n]

		decodeAndSend(ss.ctx, ss.llp, ss.pathname, n, &b, ss.partial)

		// return on any error including EOF
		if err != nil {
			if totalBytes > 0 {
				// Don't bother updating lastRead until we return. // TODO: bug?
				ss.lastReadTime = time.Now()
			}
			return err
		}
	}
}
