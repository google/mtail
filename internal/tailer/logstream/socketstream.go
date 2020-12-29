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
	"github.com/google/mtail/internal/waker"
)

type socketStream struct {
	ctx context.Context
	llp logline.Processor

	pathname string // Given name for the underlying socket path on the filesystem

	mu           sync.RWMutex // protects following fields
	completed    bool         // This pipestream is completed and can no longer be used.
	lastReadTime time.Time    // Last time a log line was read from this named pipe

	stopOnce sync.Once     // Ensure stopChan only closed once.
	stopChan chan struct{} // Close to start graceful shutdown.
}

func newSocketStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo, llp logline.Processor) (LogStream, error) {
	ss := &socketStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp, stopChan: make(chan struct{})}
	if err := ss.stream(ctx, wg, waker, fi); err != nil {
		return nil, err
	}
	return ss, nil
}

func (ss *socketStream) LastReadTime() time.Time {
	ss.mu.RLock()
	defer ss.mu.RUnlock()
	return ss.lastReadTime
}

func (ss *socketStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, fi os.FileInfo) error {
	c, err := net.ListenUnixgram("unixgram", &net.UnixAddr{ss.pathname, "unixgram"})
	if err != nil {
		logErrors.Add(ss.pathname, 1)
		return err
	}
	glog.V(2).Infof("opened new socket %v", c)
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			err := c.Close()
			if err != nil {
				logErrors.Add(ss.pathname, 1)
				glog.Info(err)
			}
			ss.mu.Lock()
			ss.completed = true
			ss.mu.Unlock()
		}()
		b := make([]byte, 0, defaultReadBufferSize)
		capB := cap(b)
		partial := bytes.NewBufferString("")
		for {
			if err := c.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
				glog.V(2).Infof("%s: %s", ss.pathname, err)
			}

			n, err := c.Read(b[:capB])
			if err, ok := err.(net.Error); ok && err.Timeout() {
				// Like pipestream, if timeout then sleep and wait for a context
				// cancellation.
				goto Sleep
			}
			// EOF means socket closed, so this socketstream is now completed.
			// All other errors also finish the stream and are counted.
			if err != nil {
				if err != io.EOF {
					glog.Info(err)
					logErrors.Add(ss.pathname, 1)
				}
				return
			}
			if err != nil {
				return
			}

			decodeAndSend(ss.ctx, ss.llp, ss.pathname, n, b[:n], partial)

			if n > 0 {
				ss.mu.Lock()
				ss.lastReadTime = time.Now()
				ss.mu.Unlock()
			}
		Sleep:
			select {
			case <-ss.stopChan:
				return
			case <-ctx.Done():
				return
			case <-waker.Wake():
				// sleep to next Wake()
			}
		}
	}()
	return nil
}

func (ss *socketStream) IsComplete() bool {
	ss.mu.RLock()
	defer ss.mu.RUnlock()
	return ss.completed
}
func (ss *socketStream) Stop() {
	ss.stopOnce.Do(func() {
		close(ss.stopChan)
	})
}
