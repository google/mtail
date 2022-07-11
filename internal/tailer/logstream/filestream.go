// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"errors"
	"expvar"
	"io"
	"os"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

// fileTruncates counts the truncations of a file stream.
var fileTruncates = expvar.NewMap("file_truncates_total")

// fileStream streams log lines from a regular file on the file system.  These
// log files are appended to by another process, and are either rotated or
// truncated by that (or yet another) process.  Rotation implies that a new
// inode with the same name has been created, the old file descriptor will be
// valid until EOF at which point it's considered completed.  A truncation means
// the same file descriptor is used but the file offset will be reset to 0.
// The latter is potentially lossy as far as mtail is concerned, if the last
// logs are not read before truncation occurs.  When an EOF is read, the
// goroutine tests for both truncation and inode change and resets or spins off
// a new goroutine and closes itself down.  The shared context is used for
// cancellation.
type fileStream struct {
	ctx   context.Context
	lines chan<- *logline.LogLine

	pathname string // Given name for the underlying file on the filesystem

	mu           sync.RWMutex // protects following fields.
	lastReadTime time.Time    // Last time a log line was read from this file
	completed    bool         // The filestream is completed and can no longer be used.

	stopOnce sync.Once     // Ensure stopChan only closed once.
	stopChan chan struct{} // Close to start graceful shutdown.
}

// newFileStream creates a new log stream from a regular file.
func newFileStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo, lines chan<- *logline.LogLine, streamFromStart bool) (LogStream, error) {
	fs := &fileStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), lines: lines, stopChan: make(chan struct{})}
	if err := fs.stream(ctx, wg, waker, fi, streamFromStart); err != nil {
		return nil, err
	}
	return fs, nil
}

func (fs *fileStream) LastReadTime() time.Time {
	fs.mu.RLock()
	defer fs.mu.RUnlock()
	return fs.lastReadTime
}

func (fs *fileStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, fi os.FileInfo, streamFromStart bool) error {
	fd, err := os.OpenFile(fs.pathname, os.O_RDONLY, 0o600)
	if err != nil {
		logErrors.Add(fs.pathname, 1)
		return err
	}
	logOpens.Add(fs.pathname, 1)
	glog.V(2).Infof("%v: opened new file", fd)
	if !streamFromStart {
		if _, err := fd.Seek(0, io.SeekEnd); err != nil {
			logErrors.Add(fs.pathname, 1)
			if err := fd.Close(); err != nil {
				logErrors.Add(fs.pathname, 1)
				glog.Info(err)
			}
			return err
		}
		glog.V(2).Infof("%v: seeked to end", fd)
	}
	b := make([]byte, defaultReadBufferSize)
	var lastBytes []byte
	partial := bytes.NewBufferString("")
	started := make(chan struct{})
	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("%v: read total %d bytes from %s", fd, total, fs.pathname)
			glog.V(2).Infof("%v: closing file descriptor", fd)
			if err := fd.Close(); err != nil {
				logErrors.Add(fs.pathname, 1)
				glog.Info(err)
			}
			logCloses.Add(fs.pathname, 1)
		}()
		close(started)
		for {
			// Blocking read but regular files will return EOF straight away.
			count, err := fd.Read(b)
			glog.V(2).Infof("%v: read %d bytes, err is %v", fd, count, err)

			if count > 0 {
				total += count
				glog.V(2).Infof("%v: decode and send", fd)
				needSend := lastBytes
				needSend = append(needSend, b[:count]...)
				sendCount := decodeAndSend(ctx, fs.lines, fs.pathname, len(needSend), needSend, partial)
				if sendCount < len(needSend) {
					lastBytes = append([]byte{}, needSend[sendCount:]...)
				} else {
					lastBytes = []byte{}
				}
				fs.mu.Lock()
				fs.lastReadTime = time.Now()
				fs.mu.Unlock()
			}

			if err != nil && err != io.EOF {
				logErrors.Add(fs.pathname, 1)
				// TODO: This could be generalised to check for any retryable
				// errors, and end on unretriables; e.g. ESTALE looks
				// retryable.
				if errors.Is(err, syscall.ESTALE) {
					glog.Infof("%v: reopening stream due to %s", fd, err)
					if nerr := fs.stream(ctx, wg, waker, fi, true); nerr != nil {
						glog.Info(nerr)
					}
					// Close this stream.
					return
				}
				glog.Info(err)
			}

			// If we have read no bytes and are at EOF, check for truncation and rotation.
			if err == io.EOF && count == 0 {
				glog.V(2).Infof("%v: eof an no bytes", fd)
				// Both rotation and truncation need to stat, so check for
				// rotation first.  It is assumed that rotation is the more
				// common change pattern anyway.
				newfi, serr := os.Stat(fs.pathname)
				if serr != nil {
					glog.Info(serr)
					// If this is a NotExist error, then we should wrap up this
					// goroutine. The Tailer will create a new logstream if the
					// file is in the middle of a rotation and gets recreated
					// in the next moment.  We can't rely on the Tailer to tell
					// us we're deleted because the tailer can only tell us to
					// Stop, which ends up causing us to race here against
					// detection of IsCompleted.
					if os.IsNotExist(serr) {
						glog.V(2).Infof("%v: source no longer exists, exiting", fd)
						if partial.Len() > 0 {
							sendLine(ctx, fs.pathname, partial, fs.lines)
						}
						fs.mu.Lock()
						fs.completed = true
						fs.mu.Unlock()
						return
					}
					logErrors.Add(fs.pathname, 1)
					goto Sleep
				}
				if !os.SameFile(fi, newfi) {
					glog.V(2).Infof("%v: adding a new file routine", fd)
					if err := fs.stream(ctx, wg, waker, newfi, true); err != nil {
						glog.Info(err)
					}
					// We're at EOF so there's nothing left to read here.
					return
				}
				currentOffset, serr := fd.Seek(0, io.SeekCurrent)
				if serr != nil {
					logErrors.Add(fs.pathname, 1)
					glog.Info(serr)
					continue
				}
				glog.V(2).Infof("%v: current seek is %d", fd, currentOffset)
				glog.V(2).Infof("%v: new size is %d", fd, newfi.Size())
				// We know that newfi is from the current file.  Truncation can
				// only be detected if the new file is currently shorter than
				// the current seek offset.  In test this can be a race, but in
				// production it's unlikely that a new file writes more bytes
				// than the previous after rotation in the time it takes for
				// mtail to notice.
				if newfi.Size() < currentOffset {
					glog.V(2).Infof("%v: truncate? currentoffset is %d and size is %d", fd, currentOffset, newfi.Size())
					// About to lose all remaining data because of the truncate so flush the accumulator.
					if partial.Len() > 0 {
						sendLine(ctx, fs.pathname, partial, fs.lines)
					}
					p, serr := fd.Seek(0, io.SeekStart)
					if serr != nil {
						logErrors.Add(fs.pathname, 1)
						glog.Info(serr)
					}
					glog.V(2).Infof("%v: Seeked to %d", fd, p)
					fileTruncates.Add(fs.pathname, 1)
					continue
				}
			}

			// No error implies there is more to read in this file so go
			// straight back to read unless it looks like context is Done.
			if err == nil && ctx.Err() == nil {
				continue
			}

		Sleep:
			// If we get here it's because we've stalled.  First test to see if it's
			// time to exit.
			if err == io.EOF || ctx.Err() != nil {
				select {
				case <-fs.stopChan:
					glog.V(2).Infof("%v: stream has been stopped, exiting", fd)
					if partial.Len() > 0 {
						sendLine(ctx, fs.pathname, partial, fs.lines)
					}
					fs.mu.Lock()
					fs.completed = true
					fs.mu.Unlock()
					return
				case <-ctx.Done():
					glog.V(2).Infof("%v: stream has been cancelled, exiting", fd)
					if partial.Len() > 0 {
						sendLine(ctx, fs.pathname, partial, fs.lines)
					}
					fs.mu.Lock()
					fs.completed = true
					fs.mu.Unlock()
					return
				default:
					// keep going
				}
			}

			// Don't exit, instead yield and wait for a termination signal or
			// wakeup.
			glog.V(2).Infof("%v: waiting", fd)
			select {
			case <-fs.stopChan:
				// We may have started waiting here when the stop signal
				// arrives, but since that wait the file may have been
				// written to.  The file is not technically yet at EOF so
				// we need to go back and try one more read.  We'll exit
				// the stream in the select stanza above.
				glog.V(2).Infof("%v: Stopping after next read", fd)
			case <-ctx.Done():
				// Same for cancellation; this makes tests stable, but
				// could argue exiting immediately is less surprising.
				// Assumption is that this doesn't make a difference in
				// production.
				glog.V(2).Infof("%v: Cancelled after next read", fd)
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("%v: Wake received", fd)
			}
		}
	}()

	<-started
	return nil
}

func (fs *fileStream) IsComplete() bool {
	fs.mu.RLock()
	defer fs.mu.RUnlock()
	return fs.completed
}

// Stop implements the LogStream interface.
func (fs *fileStream) Stop() {
	fs.stopOnce.Do(func() {
		glog.Info("signalling stop at next EOF")
		close(fs.stopChan)
	})
}
