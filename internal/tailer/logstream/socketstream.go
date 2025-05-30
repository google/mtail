// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"context"
	"fmt"
	"net"
	"sync"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/waker"
)

type socketStream struct {
	streamBase

	cancel context.CancelFunc

	oneShot OneShotMode
	scheme  string // URL Scheme to listen with, either tcp or unix
	address string // Given name for the underlying socket path on the filesystem or host/port.
}

func newSocketStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, scheme, address string, oneShot OneShotMode) (LogStream, error) {
	if address == "" {
		return nil, ErrEmptySocketAddress
	}
	ctx, cancel := context.WithCancel(ctx)
	ss := &socketStream{
		cancel:  cancel,
		oneShot: oneShot,
		scheme:  scheme,
		address: address,
		streamBase: streamBase{
			sourcename: fmt.Sprintf("%s://%s", scheme, address),
			lines:      make(chan *logline.LogLine),
		},
	}

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
	glog.V(2).Infof("stream(%s): opened new socket listener %+v", ss.sourcename, l)

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
		glog.V(2).Infof("stream(%s): closing listener", ss.sourcename)
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
			glog.V(2).Infof("stream(%s): got new conn %v", ss.sourcename, c)
			connWg.Add(1)
			go ss.handleConn(ctx, &connWg, waker, c)
			connOnce.Do(func() { close(started) })
			if ss.oneShot {
				glog.Infof("stream(%s): oneshot mode, exiting accept loop", ss.sourcename)
				return
			}
		}
	}()

	return nil
}

func (ss *socketStream) handleConn(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, c net.Conn) {
	defer wg.Done()

	lr := NewLineReader(ss.sourcename, ss.lines, c, defaultReadBufferSize, ss.cancel)
	var total int
	defer func() {
		glog.V(2).Infof("stream(%s): read total %d bytes from %s", ss.sourcename, c, total)
		glog.V(2).Infof("stream(%s): closing connection, %v", ss.sourcename, c)
		err := c.Close()
		if err != nil {
			logErrors.Add(ss.address, 1)
			glog.Info(err)
		}
		lr.Finish(ctx)
		logCloses.Add(ss.address, 1)
	}()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	SetReadDeadlineOnDone(ctx, c)

	for {
		n, err := lr.ReadAndSend(ctx)
		glog.V(2).Infof("stream(%s): read %d bytes, err is %v", ss.sourcename, n, err)

		if n > 0 {
			total += n

			// No error implies more to read, so restart the loop.
			if err == nil && ctx.Err() == nil {
				continue
			}
		}

		if IsExitableError(err) {
			glog.V(2).Infof("stream(%s): exiting, conn has error %s", ss.sourcename, err)
			return
		}

		// Yield and wait
		glog.V(2).Infof("stream(%s): waiting", ss.sourcename)
		select {
		case <-ctx.Done():
			// Exit after next read attempt.
			glog.V(2).Infof("stream(%s:%s): context cancelled, exiting after next read timeout", ss.scheme, ss.address)
		case <-waker.Wake():
			// sleep until next Wake()
			glog.V(2).Infof("stream(%s): Wake received", ss.sourcename)
		}
	}
}
