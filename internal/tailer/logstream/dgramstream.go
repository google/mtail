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
		}()
		ctx, cancel := context.WithCancel(ctx)
		defer cancel()
		SetReadDeadlineOnDone(ctx, c)

		for {
			n, err := c.Read(b)
			glog.V(2).Infof("%v: read %d bytes, err is %v", c, n, err)

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
				ss.mu.Lock()
				ss.completed = true
				ss.mu.Unlock()
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
				// the stream in the select stanza above.
				glog.V(2).Infof("%v: Stopping after next read timeout", c)
			case <-ctx.Done():
				// Same for cancellation; this makes tests stable, but
				// could argue exiting immediately is less surprising.
				// Assumption is that this doesn't make a difference in
				// production.
				glog.V(2).Infof("%v: Cancelled after next read", c)
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("%v: Wake received", c)
			}

			// There's no EOF so set a short deadline on the next read.  If we
			// get bytes, we're not done, otherwise we'll exit.  We must check
			// again here to avoid racing with the waker above.
			select {
			case <-ss.stopChan:
				c.SetReadDeadline(time.Now().Add(100 * time.Millisecond))
			default:
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
