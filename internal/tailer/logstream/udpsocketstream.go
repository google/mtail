// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"errors"
	"io"
	"net"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

type udpSocketStream struct {
	ctx   context.Context
	lines chan<- *logline.LogLine

	hostspec string // Host and optionally port to bind to

	mu           sync.RWMutex // protects following fields
	completed    bool         // This pipestream is completed and can no longer be used.
	lastReadTime time.Time    // Last time a log line was read from this named pipe

	stopOnce sync.Once     // Ensure stopChan only closed once.
	stopChan chan struct{} // Close to start graceful shutdown.
}

func newUdpSocketStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, hostspec string, lines chan<- *logline.LogLine) (LogStream, error) {
	ss := &udpSocketStream{ctx: ctx, hostspec: hostspec, lastReadTime: time.Now(), lines: lines, stopChan: make(chan struct{})}
	if err := ss.stream(ctx, wg, waker); err != nil {
		return nil, err
	}
	return ss, nil
}

func (ss *udpSocketStream) LastReadTime() time.Time {
	ss.mu.RLock()
	defer ss.mu.RUnlock()
	return ss.lastReadTime
}

//func (ss *udpSocketStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, fi os.FileInfo) error {
func (ss *udpSocketStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker) error {
	addr, err := net.ResolveUDPAddr("udp", ss.hostspec)
	if err != nil {
		logErrors.Add(ss.hostspec, 1)
		return err
	}
	c, err := net.ListenUDP("udp", addr)
	if err != nil {
		logErrors.Add(ss.hostspec, 1)
		return err
	}
	glog.V(2).Infof("opened new socket %v", c)
	wg.Add(1)
	var total int
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("%v: read total %d bytes from %s", c, total, ss.hostspec)
			glog.V(2).Infof("%v: closing connection", c)
			err := c.Close()
			if err != nil {
				logErrors.Add(ss.hostspec, 1)
				glog.Info(err)
			}
			logCloses.Add(ss.hostspec, 1)
			ss.mu.Lock()
			ss.completed = true
			ss.mu.Unlock()
		}()
		space := []byte(" ")
		b := make([]byte, 0, defaultReadBufferSize)
		capB := cap(b)
		var timedout bool
		for {
			if err := c.SetReadDeadline(time.Now().Add(defaultReadTimeout)); err != nil {
				glog.V(2).Infof("%s: %s", ss.hostspec, err)
			}

			n, err := c.Read(b[:capB])

			// Each datagram is a separate message, and we should
			// receive the full datagram on one socket read.
			if n > 0 {
				// BSD Syslog ignores a trailing space
				if bytes.Equal(b[n-1:n], space) {
					n--
				}
				total += n
				decodeAndSend(ss.ctx, ss.lines, ss.hostspec, n, b[:n], nil)
				ss.mu.Lock()
				ss.lastReadTime = time.Now()
				ss.mu.Unlock()
			}

			var nerr net.Error
			if errors.As(err, &nerr) && nerr.Timeout() && n == 0 {
				// Like pipestream, if timeout then sleep and wait for a context
				// cancellation.
				timedout = true
				goto Sleep
			}
			// EOF means socket closed, so this socketstream is now completed.
			// All other errors also finish the stream and are counted.
			if err != nil {
				if err != io.EOF {
					glog.Info(err)
					logErrors.Add(ss.hostspec, 1)
				}
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
				case <-ss.stopChan:
					glog.V(2).Infof("%v: stream has been stopped, exiting", c)
					ss.mu.Lock()
					ss.completed = true
					ss.mu.Unlock()
					return
				case <-ctx.Done():
					glog.V(2).Infof("%v: context has been cancelled, exiting", c)
					ss.mu.Lock()
					ss.completed = true
					ss.mu.Unlock()
					return
				default:
					// keep going
				}
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
				glog.V(2).Infof("%v: Stopping after next read", c)
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
		}
	}()
	return nil
}

func (ss *udpSocketStream) IsComplete() bool {
	ss.mu.RLock()
	defer ss.mu.RUnlock()
	return ss.completed
}

func (ss *udpSocketStream) Stop() {
	ss.stopOnce.Do(func() {
		close(ss.stopChan)
	})
}
