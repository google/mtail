// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"io"
	"net"
	"os"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
)

type socketStream struct {
	ctx          context.Context
	pathname     string    // Given name for the underlying socket path on the filesystem
	lastReadTime time.Time // Last time a log line was read from this socket
	c            net.Conn  // Connection for the open socket
	llp          logline.Processor
	wakeChannel  chan struct{}

	finishedMu sync.Mutex // protects `finished`
	finished   bool       // The pipestream is finished and can no longer be used.
}

func newSocketStream(ctx context.Context, wg *sync.WaitGroup, pathname string, fi os.FileInfo, llp logline.Processor) (LogStream, error) {
	ss := &socketStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp, wakeChannel: make(chan struct{}, 1)}
	wg.Add(1)
	go ss.read(ctx, wg, fi)
	return ss, nil
}

func (ss *socketStream) LastReadTime() time.Time {
	return ss.lastReadTime
}

func (ss *socketStream) Wake() {
	ss.wakeChannel <- struct{}{}
}

func (ss *socketStream) read(ctx context.Context, wg *sync.WaitGroup, fi os.FileInfo) {
	defer wg.Done()
	c, err := net.ListenUnixgram("unixgram", &net.UnixAddr{ss.pathname, "unixgram"})
	if err != nil {
		glog.Info(err)
	}
	defer func() {
		err := c.Close()
		if err != nil {
			glog.Info(err)
		}
	}()
	b := make([]byte, 0, defaultReadBufferSize)
	capB := cap(b)
	partial := bytes.NewBufferString("")
	for {
		if err := ss.c.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
			glog.V(2).Infof("%s: %s", ss.pathname, err)
		}

		n, err := ss.c.Read(b[:capB])
		if err, ok := err.(net.Error); ok && err.Timeout() {
			// Like pipestream, if timeout then sleep and wait for a context
			// cancellation.
			goto Sleep
		}
		// EOF means socket closed, so this socketstream is now finished.
		if err == io.EOF {
			ss.finishedMu.Lock()
			ss.finished = true
			ss.finishedMu.Unlock()
			return
		}

		decodeAndSend(ss.ctx, ss.llp, ss.pathname, n, b[:n], partial)

		if n > 0 {
			// Don't bother updating lastRead until we return. // TODO: bug?
			ss.lastReadTime = time.Now()
		}
	Sleep:
		select {
		case <-ss.wakeChannel:
			// sleep to next Wake()
		case <-ctx.Done():
			return
		}
	}
}

func (ss *socketStream) IsFinished() bool {
	ss.finishedMu.Lock()
	defer ss.finishedMu.Unlock()
	return ss.finished
}
