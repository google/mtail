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

	scheme  string // Datagram scheme, either "unixgram" or "udp".
	address string // Given name for the underlying socket path on the filesystem or hostport.

	mu           sync.RWMutex // protects following fields
	completed    bool         // This pipestream is completed and can no longer be used.
	lastReadTime time.Time    // Last time a log line was read from this named pipe

	stopOnce sync.Once     // Ensure stopChan only closed once.
	stopChan chan struct{} // Close to start graceful shutdown.
}

func newDgramStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, scheme, address string, lines chan<- *logline.LogLine, oneShot OneShotMode) (LogStream, error) {
	if address == "" {
		return nil, ErrEmptySocketAddress
	}
	ss := &dgramStream{ctx: ctx, scheme: scheme, address: address, lastReadTime: time.Now(), lines: lines, stopChan: make(chan struct{})}
	if err := ss.stream(ctx, wg, waker, oneShot); err != nil {
		return nil, err
	}
	return ss, nil
}

func (ss *dgramStream) LastReadTime() time.Time {
	ss.mu.RLock()
	defer ss.mu.RUnlock()
	return ss.lastReadTime
}

// The read buffer size for datagrams.
const datagramReadBufferSize = 131072

func (ss *dgramStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, oneShot OneShotMode) error {
	c, err := net.ListenPacket(ss.scheme, ss.address)
	if err != nil {
		logErrors.Add(ss.address, 1)
		return err
	}
	glog.V(2).Infof("stream(%s:%s): opened new datagram socket %v", ss.scheme, ss.address, c)
	b := make([]byte, datagramReadBufferSize)
	partial := bytes.NewBufferString("")
	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("stream(%s:%s): read total %d bytes", ss.scheme, ss.address, total)
			glog.V(2).Infof("stream(%s:%s): closing connection", ss.scheme, ss.address)
			err := c.Close()
			if err != nil {
				logErrors.Add(ss.address, 1)
				glog.Info(err)
			}
			logCloses.Add(ss.address, 1)
			ss.mu.Lock()
			ss.completed = true
			ss.mu.Unlock()
			ss.Stop()
		}()
		ctx, cancel := context.WithCancel(ctx)
		defer cancel()
		SetReadDeadlineOnDone(ctx, c)

		for {
			n, _, err := c.ReadFrom(b)
			glog.V(2).Infof("stream(%s:%s): read %d bytes, err is %v", ss.scheme, ss.address, n, err)

			// This is a test-only trick that says if we've already put this
			// logstream in graceful shutdown, then a zero-byte read is
			// equivalent to an "EOF" in connection and file oriented streams.
			if n == 0 {
				if oneShot {
					glog.V(2).Infof("stream(%s:%s): exiting because zero byte read and one shot", ss.scheme, ss.address)
					if partial.Len() > 0 {
						sendLine(ctx, ss.address, partial, ss.lines)
					}
					return
				}
				select {
				case <-ss.stopChan:
					glog.V(2).Infof("stream(%s:%s): exiting because zero byte read after Stop", ss.scheme, ss.address)
					return
				default:
				}
			}

			if n > 0 {
				total += n
				//nolint:contextcheck
				decodeAndSend(ss.ctx, ss.lines, ss.address, n, b[:n], partial)
				ss.mu.Lock()
				ss.lastReadTime = time.Now()
				ss.mu.Unlock()
			}

			if err != nil && IsEndOrCancel(err) {
				if partial.Len() > 0 {
					sendLine(ctx, ss.address, partial, ss.lines)
				}
				glog.V(2).Infof("stream(%s:%s): exiting, stream has error %s", ss.scheme, ss.address, err)
				return
			}

			// Yield and wait
			glog.V(2).Infof("stream(%s:%s): waiting", ss.scheme, ss.address)
			select {
			case <-ss.stopChan:
				// We may have started waiting here when the stop signal
				// arrives, but since that wait the file may have been
				// written to.  The file is not technically yet at EOF so
				// we need to go back and try one more read.  We'll exit
				// the stream in the zero byte handler above.
				glog.V(2).Infof("stream(%s:%s): Stopping after next zero byte read", ss.scheme, ss.address)
			case <-ctx.Done():
				// Exit immediately; a cancelled context will set an immediate
				// deadline on the next read which will cause us to exit then,
				// so don't bother going around the loop again.
				return
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("stream(%s:%s): Wake received", ss.scheme, ss.address)
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
	glog.V(2).Infof("stream(%s:%s): Stop received on datagram stream.", ss.scheme, ss.address)
	ss.stopOnce.Do(func() {
		close(ss.stopChan)
	})
}
