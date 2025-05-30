// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"context"
	"errors"
	"io"
	"os"
	"sync"
	"syscall"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/waker"
)

type fifoStream struct {
	streamBase

	cancel context.CancelFunc

	pathname string // Given name for the underlying named pipe on the filesystem
}

// newFifoStream creates a new stream reader for Unix Fifos.
// `pathname` must already be verified as clean.
func newFifoStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo) (LogStream, error) {
	ctx, cancel := context.WithCancel(ctx)
	ps := &fifoStream{
		cancel:   cancel,
		pathname: pathname,
		streamBase: streamBase{
			sourcename: pathname,
			lines:      make(chan *logline.LogLine),
		},
	}
	if err := ps.stream(ctx, wg, waker, fi); err != nil {
		return nil, err
	}
	return ps, nil
}

func fifoOpen(pathname string) (*os.File, error) {
	if IsStdinPattern(pathname) {
		return os.Stdin, nil
	}
	// Open in nonblocking mode because the write end of the fifo may not have started yet; this also gives us the ability to set a read deadline when the context is cancelled. https://github.com/golang/go/issues/24842
	fd, err := os.OpenFile(pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0o600) // #nosec G304 -- path already validated by caller
	if err != nil {
		glog.Warningf("fifoOpen(%s): open failed: %v", pathname, err)
		logErrors.Add(pathname, 1)
		return nil, err
	}
	glog.V(2).Infof("fifoOpen(%s): opened new fifo %v", pathname, fd)
	return fd, nil
}

// The read buffer size for fifos.
//
// Before Linux 2.6.11, the capacity of a fifo was the same as the
// system page size (e.g., 4096 bytes on i386).  Since Linux 2.6.11,
// the fifo capacity is 16 pages (i.e., 65,536 bytes in a system
// with a page size of 4096 bytes).  Since Linux 2.6.35, the default
// fifo capacity is 16 pages, but the capacity can be queried and
// set using the fcntl(2) F_GETPIPE_SZ and F_SETPIPE_SZ operations.
// See fcntl(2) for more information.
//
// https://man7.org/linux/man-pages/man7/pipe.7.html
const defaultFifoReadBufferSize = 131072

func (ps *fifoStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, _ os.FileInfo) error {
	fd, err := fifoOpen(ps.pathname)
	if err != nil {
		return err
	}
	lr := NewLineReader(ps.sourcename, ps.lines, fd, defaultFifoReadBufferSize, ps.cancel)
	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("stream(%s): read total %d bytes", ps.sourcename, total)
			glog.V(2).Infof("stream(%s): closing file descriptor %v", ps.sourcename, fd)
			err := fd.Close()
			if err != nil {
				logErrors.Add(ps.pathname, 1)
				glog.Info(err)
			}
			logCloses.Add(ps.pathname, 1)
			lr.Finish(ctx)
			close(ps.lines)
			ps.cancel()
		}()
		SetReadDeadlineOnDone(ctx, fd)

		for {
			n, err := lr.ReadAndSend(ctx)

			if n > 0 {
				total += n

				// No error implies there is more to read so restart the loop.
				if err == nil && ctx.Err() == nil {
					continue
				}
			} else if n == 0 && total > 0 {
				// `pipe(7)` tells us "If all file descriptors referring to the
				// write end of a fifo have been closed, then an attempt to
				// read(2) from the fifo will see end-of-file (read(2) will
				// return 0)."  To avoid shutting down the stream at startup
				// before any writer has connected to the fifo, condition on
				// having read any bytes previously.
				glog.V(2).Infof("stream(%s): exiting, 0 bytes read", ps.sourcename)
				return
			}

			// Test to see if we should exit.
			if IsExitableError(err) {
				// Because we've opened in nonblocking mode, this Read can return
				// straight away.  If there are no writers, it'll return EOF (per
				// `pipe(7)` and `read(2)`.)  This is expected when `mtail` is
				// starting at system init as the writer may not be ready yet.
				if !(errors.Is(err, io.EOF) && total == 0) {
					glog.V(2).Infof("stream(%s): exiting, stream has error %s", ps.sourcename, err)
					return
				}
			}

			// Wait for wakeup or termination.
			glog.V(2).Infof("stream(%s): waiting", ps.sourcename)
			select {
			case <-ctx.Done():
				// Exit after next read attempt.
				glog.V(2).Infof("stream(%s): context cancelled, exiting after next read timeout", ps.pathname)
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("stream(%s): Wake received", ps.sourcename)
			}
		}
	}()
	return nil
}
