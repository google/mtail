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

type socketStream struct {
	streamBase

	cancel context.CancelFunc

	oneShot OneShotMode
	scheme  string // URL Scheme to listen with, either tcp or unix
	address string // Given name for the underlying socket path on the filesystem or host/port.

	mu           sync.RWMutex // protects following fields
	lastReadTime time.Time    // Last time a log line was read from this socket

	staleTimer *time.Timer // Expire the stream if no read in 24h
}

func newSocketStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, scheme, address string, oneShot OneShotMode) (LogStream, error) {
	if address == "" {
		return nil, ErrEmptySocketAddress
	}
	ctx, cancel := context.WithCancel(ctx)
	ss := &socketStream{cancel: cancel, oneShot: oneShot, scheme: scheme, address: address, lastReadTime: time.Now(), streamBase: streamBase{lines: make(chan *logline.LogLine)}}
	if err := ss.stream(ctx, wg, waker); err != nil {
		return nil, err
	}
	return ss, nil
}

// stream starts goroutines to read data from the stream socket, until Stop is called or the context is cancelled.
func (ss *socketStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker) error {
	l, err := net.Listen(ss.scheme, ss.address)
	if err != nil {
		logErrors.Add(ss.address, 1)
		return err
	}
	glog.V(2).Infof("stream(%s:%s): opened new socket listener %+v", ss.scheme, ss.address, l)

	// signals when a connection has been opened
	started := make(chan struct{})
	// tracks connection handling routines
	var connWg sync.WaitGroup

	// Set up for shutdown
	wg.Add(1)
	go func() {
		defer wg.Done()
		// If oneshot, wait only for the one conn handler to start, otherwise
		// wait for context Done or stopChan.
		<-started
		if !ss.oneShot {
			<-ctx.Done()
		}
		glog.V(2).Infof("stream(%s:%s): closing listener", ss.scheme, ss.address)
		err := l.Close()
		if err != nil {
			glog.Info(err)
		}
		connWg.Wait()
		close(ss.lines)
	}()

	var connOnce sync.Once

	wg.Add(1)
	go func() {
		defer wg.Done()
		for {
			c, err := l.Accept()
			if err != nil {
				glog.Info(err)
				return
			}
			glog.V(2).Infof("stream(%s:%s): got new conn %v", ss.scheme, ss.address, c)
			connWg.Add(1)
			go ss.handleConn(ctx, &connWg, waker, c)
			connOnce.Do(func() { close(started) })
			if ss.oneShot {
				glog.Infof("stream(%s:%s): oneshot mode, exiting accept loop", ss.scheme, ss.address)
				return
			}
		}
	}()

	return nil
}

func (ss *socketStream) handleConn(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, c net.Conn) {
	defer wg.Done()
	b := make([]byte, defaultReadBufferSize)
	partial := bytes.NewBufferString("")
	var total int
	defer func() {
		glog.V(2).Infof("stream(%s:%s): read total %d bytes from %s", ss.scheme, ss.address, c, total)
		glog.V(2).Infof("stream(%s:%s): closing connection, %v", ss.scheme, ss.address, c)
		err := c.Close()
		if err != nil {
			logErrors.Add(ss.address, 1)
			glog.Info(err)
		}
		logCloses.Add(ss.address, 1)
	}()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	SetReadDeadlineOnDone(ctx, c)

	for {
		n, err := c.Read(b)
		glog.V(2).Infof("stream(%s:%s): read %d bytes, err is %v", ss.scheme, ss.address, n, err)

		if ss.staleTimer != nil {
			ss.staleTimer.Stop()
		}

		if n > 0 {
			total += n
			//nolint:contextcheck
			decodeAndSend(ctx, ss.lines, ss.address, n, b[:n], partial)
			ss.mu.Lock()
			ss.lastReadTime = time.Now()
			ss.mu.Unlock()
			ss.staleTimer = time.AfterFunc(time.Hour*24, ss.cancel)

			// No error implies more to read, so restart the loop.
			if err == nil && ctx.Err() == nil {
				continue
			}
		}

		if err != nil && IsEndOrCancel(err) {
			if partial.Len() > 0 {
				sendLine(ctx, ss.address, partial, ss.lines)
			}
			glog.V(2).Infof("stream(%s:%s): exiting, conn has error %s", ss.scheme, ss.address, err)

			return
		}

		// Yield and wait
		glog.V(2).Infof("stream(%s:%s): waiting", ss.scheme, ss.address)
		select {
		case <-ctx.Done():
			// Exit after next read attempt.
			glog.V(2).Infof("stream(%s:%s): context cancelled, exiting after next read timeout", ss.scheme, ss.address)
		case <-waker.Wake():
			// sleep until next Wake()
			glog.V(2).Infof("stream(%s:%s): Wake received", ss.scheme, ss.address)
		}
	}
}
