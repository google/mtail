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
	cancel context.CancelFunc

	lines chan *logline.LogLine

	scheme  string // Datagram scheme, either "unixgram" or "udp".
	address string // Given name for the underlying socket path on the filesystem or hostport.

	mu           sync.RWMutex // protects following fields
	completed    bool         // This pipestream is completed and can no longer be used.
	lastReadTime time.Time    // Last time a log line was read from this named pipe
}

func newDgramStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, scheme, address string, oneShot OneShotMode) (LogStream, error) {
	if address == "" {
		return nil, ErrEmptySocketAddress
	}
	ctx, cancel := context.WithCancel(ctx)
	ss := &dgramStream{cancel: cancel, scheme: scheme, address: address, lastReadTime: time.Now(), lines: make(chan *logline.LogLine)}
	if err := ss.stream(ctx, wg, waker, oneShot); err != nil {
		return nil, err
	}
	return ss, nil
}

func (ds *dgramStream) LastReadTime() time.Time {
	ds.mu.RLock()
	defer ds.mu.RUnlock()
	return ds.lastReadTime
}

// The read buffer size for datagrams.
const datagramReadBufferSize = 131072

func (ds *dgramStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, oneShot OneShotMode) error {
	c, err := net.ListenPacket(ds.scheme, ds.address)
	if err != nil {
		logErrors.Add(ds.address, 1)
		return err
	}
	glog.V(2).Infof("stream(%s:%s): opened new datagram socket %v", ds.scheme, ds.address, c)
	b := make([]byte, datagramReadBufferSize)
	partial := bytes.NewBufferString("")
	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("stream(%s:%s): read total %d bytes", ds.scheme, ds.address, total)
			glog.V(2).Infof("stream(%s:%s): closing connection", ds.scheme, ds.address)
			err := c.Close()
			if err != nil {
				logErrors.Add(ds.address, 1)
				glog.Info(err)
			}
			logCloses.Add(ds.address, 1)
			ds.mu.Lock()
			ds.completed = true
			close(ds.lines)
			ds.mu.Unlock()
			ds.Stop()
		}()
		ctx, cancel := context.WithCancel(ctx)
		defer cancel()
		SetReadDeadlineOnDone(ctx, c)

		for {
			n, _, err := c.ReadFrom(b)
			glog.V(2).Infof("stream(%s:%s): read %d bytes, err is %v", ds.scheme, ds.address, n, err)

			// This is a test-only trick that says if we've already put this
			// logstream in graceful shutdown, then a zero-byte read is
			// equivalent to an "EOF" in connection and file oriented streams.
			if n == 0 {
				if oneShot {
					glog.V(2).Infof("stream(%s:%s): exiting because zero byte read and one shot", ds.scheme, ds.address)
					if partial.Len() > 0 {
						sendLine(ctx, ds.address, partial, ds.lines)
					}
					return
				}
				select {
				case <-ctx.Done():
					glog.V(2).Infof("stream(%s:%s): exiting because zero byte read after cancellation", ds.scheme, ds.address)
					if partial.Len() > 0 {
						sendLine(ctx, ds.address, partial, ds.lines)
					}
					return
				default:
				}
			}

			if n > 0 {
				total += n
				//nolint:contextcheck
				decodeAndSend(ctx, ds.lines, ds.address, n, b[:n], partial)
				ds.mu.Lock()
				ds.lastReadTime = time.Now()
				ds.mu.Unlock()
			}

			if err != nil && IsEndOrCancel(err) {
				if partial.Len() > 0 {
					sendLine(ctx, ds.address, partial, ds.lines)
				}
				glog.V(2).Infof("stream(%s:%s): exiting, stream has error %s", ds.scheme, ds.address, err)
				return
			}

			// Yield and wait
			glog.V(2).Infof("stream(%s:%s): waiting", ds.scheme, ds.address)
			select {
			case <-ctx.Done():
				// We may have started waiting here when the stop signal
				// arrives, but since that wait the file may have been
				// written to.  The file is not technically yet at EOF so
				// we need to go back and try one more read.  We'll exit
				// the stream in the zero byte handler above.
				glog.V(2).Infof("stream(%s:%s): Stopping after next zero byte read", ds.scheme, ds.address)
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("stream(%s:%s): Wake received", ds.scheme, ds.address)
			}
		}
	}()
	return nil
}

func (ds *dgramStream) IsComplete() bool {
	ds.mu.RLock()
	defer ds.mu.RUnlock()
	return ds.completed
}

func (ds *dgramStream) Stop() {
	glog.V(2).Infof("stream(%s:%s): Stop received on datagram stream.", ds.scheme, ds.address)
	ds.cancel()
}

// Lines implements the LogStream interface, returning the output lines channel.
func (ds *dgramStream) Lines() <-chan *logline.LogLine {
	return ds.lines
}
