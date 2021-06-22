// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"net"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

type dgramStream struct {
	ctx   context.Context
	lines chan<- *logline.LogLine

	pathname string // Given name for the underlying socket path on the filesystem

	mu           sync.RWMutex // protects following fields
	completed    bool         // This pipestream is completed and can no longer be used.
	lastReadTime time.Time    // Last time a log line was read from this named pipe

	stopOnce sync.Once     // Ensure stopChan only closed once.
	stopChan chan struct{} // Close to start graceful shutdown.
}

func newDgramStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, lines chan<- *logline.LogLine) (LogStream, error) {
	ss := &dgramStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), lines: lines, stopChan: make(chan struct{})}
	if err := ss.stream(ctx, wg, waker); err != nil {
		return nil, err
	}
	return ss, nil
}

func (ss *dgramStream) LastReadTime() time.Time {
	ss.mu.RLock()
	defer ss.mu.RUnlock()
	return ss.lastReadTime
}

const datagramReadBufferSize = 131071

func (ss *dgramStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker) error {
	c, err := net.ListenUnixgram("unixgram", &net.UnixAddr{ss.pathname, "unixgram"})
	if err != nil {
		logErrors.Add(ss.pathname, 1)
		return err
	}
	glog.V(2).Infof("opened new datagram socket %v", c)
	b := make([]byte, datagramReadBufferSize)
	partial := bytes.NewBufferString("")
	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("%v: read total %d bytes from %s", c, total, ss.pathname)
			glog.V(2).Infof("%v: closing connection", c)
			err := c.Close()
			if err != nil {
				logErrors.Add(ss.pathname, 1)
				glog.Info(err)
			}
			logCloses.Add(ss.pathname, 1)
			ss.mu.Lock()
			ss.completed = true
			ss.mu.Unlock()
			ss.Stop()
		}()
		ctx, cancel := context.WithCancel(ctx)
		defer cancel()
		SetReadDeadlineOnDone(ctx, c)

		for {
			n, err := c.Read(b)
			glog.V(2).Infof("%v: read %d bytes, err is %v", c, n, err)

			// This is a test-only trick that says if we've already put this
			// logstream in graceful shutdown, then a zero-byte read is
			// equivalent to an "EOF" in connection and file oriented streams.
			if n == 0 {
				select {
				case <-ss.stopChan:
					glog.V(2).Infof("%v: exiting because zero byte read after Stop", c)
					return
				default:
				}
			}

			if n > 0 {
				total += n
				decodeAndSend(ss.ctx, ss.lines, ss.pathname, n, b[:n], partial)
				ss.mu.Lock()
				ss.lastReadTime = time.Now()
				ss.mu.Unlock()
			}

			if err != nil && IsEndOrCancel(err) {
				if partial.Len() > 0 {
					sendLine(ctx, ss.pathname, partial, ss.lines)
				}
				glog.V(2).Infof("%v: exiting, stream has error %s", c, err)
				return
			}

			// Yield and wait
			glog.V(2).Infof("%v: waiting", c)
			select {
			case <-ss.stopChan:
				// We may have started waiting here when the stop signal
				// arrives, but since that wait the file may have been
				// written to.  The file is not technically yet at EOF so
				// we need to go back and try one more read.  We'll exit
				// the stream in the zero byte handler above.
				glog.V(2).Infof("%v: Stopping after next zero byte read", c)
			case <-ctx.Done():
				// Exit immediately; a cancelled context will set an immediate
				// deadline on the next read which will cause us to exit then,
				// so don't bother going around the loop again.
				return
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("%v: Wake received", c)
			}
		}
	}()
	return nil
}

func (ss *dgramStream) IsComplete() bool {
	ss.mu.RLock()
	defer ss.mu.RUnlock()
	return ss.completed
}

func (ss *dgramStream) Stop() {
	glog.V(2).Infof("Stop received on datagram stream.")
	ss.stopOnce.Do(func() {
		close(ss.stopChan)
	})
}
