// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"os"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

type pipeStream struct {
	streamBase

	cancel context.CancelFunc

	pathname string // Given name for the underlying named pipe on the filesystem
}

// newPipeStream creates a new stream reader for Unix Pipes.
// `pathname` must already be verified as clean.
func newPipeStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo) (LogStream, error) {
	ctx, cancel := context.WithCancel(ctx)
	ps := &pipeStream{
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

func pipeOpen(pathname string) (*os.File, error) {
	if IsStdinPattern(pathname) {
		return os.Stdin, nil
	}
	// Open in nonblocking mode because the write end of the pipe may not have started yet.
	return os.OpenFile(pathname, os.O_RDONLY|syscall.O_NONBLOCK, 0o600) // #nosec G304 -- path already validated by caller
}

// The read buffer size for pipes.
//
// Before Linux 2.6.11, the capacity of a pipe was the same as the
// system page size (e.g., 4096 bytes on i386).  Since Linux 2.6.11,
// the pipe capacity is 16 pages (i.e., 65,536 bytes in a system
// with a page size of 4096 bytes).  Since Linux 2.6.35, the default
// pipe capacity is 16 pages, but the capacity can be queried and
// set using the fcntl(2) F_GETPIPE_SZ and F_SETPIPE_SZ operations.
// See fcntl(2) for more information.
//
// https://man7.org/linux/man-pages/man7/pipe.7.html
const defaultPipeReadBufferSize = 131072

func (ps *pipeStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, _ os.FileInfo) error {
	fd, err := pipeOpen(ps.pathname)
	if err != nil {
		logErrors.Add(ps.pathname, 1)
		return err
	}
	glog.V(2).Infof("stream(%s): opened new pipe %v", ps.sourcename, fd)
	b := make([]byte, defaultPipeReadBufferSize)
	partial := bytes.NewBufferString("")
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
			if partial.Len() > 0 {
				ps.sendLine(ctx, partial)
			}
			close(ps.lines)
			ps.cancel()
		}()
		SetReadDeadlineOnDone(ctx, fd)

		for {
			n, err := fd.Read(b)
			glog.V(2).Infof("stream(%s): read %d bytes, err is %v", ps.sourcename, n, err)

			if ps.staleTimer != nil {
				ps.staleTimer.Stop()
			}

			if n > 0 {
				total += n
				ps.decodeAndSend(ctx, n, b[:n], partial)
				ps.staleTimer = time.AfterFunc(time.Hour*24, ps.cancel)

				// No error implies there is more to read so restart the loop.
				if err == nil && ctx.Err() == nil {
					continue
				}
			} else if n == 0 {
				// `pipe(7)` tells us "If all file descriptors referring to the
				// write end of a pipe have been closed, then an attempt to
				// read(2) from the pipe will see end-of-file (read(2) will
				// return 0)."
				glog.V(2).Infof("stream(%s): exiting, 0 bytes read", ps.sourcename)
				return
			}

			// Test to see if we should exit.
			if IsExitableError(err) {
				glog.V(2).Infof("stream(%s): exiting, stream has error %s", ps.sourcename, err)
				return
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
