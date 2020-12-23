// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"expvar"
	"io"
	"os"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

var (
	// fileRotations counts the rotations of a file stream
	fileRotations = expvar.NewMap("file_rotations_total")
	// fileTruncates counts the truncations of a file stream
	fileTruncates = expvar.NewMap("file_truncates_total")
)

// fileStream streams log lines from a regular file on the file system.  These
// log files are appended to by another process, and are either rotated or
// truncated by that (or yet another) process.  Rotation implies that a new
// inode with the same name has been created, the old file descriptor will be
// valid until EOF at which point it's considered finished.  A truncation means
// the same file descriptor is used but the file offset will be reset to 0.
// The latter is potentially lossy as far as mtail is concerned, if the last
// logs are not read before truncation occurs.  When an EOF is read, the
// goroutine tests for both truncation and inode change and resets or spins off
// a new goroutine and closes itself down.  The shared context is used for
// cancellation.
type fileStream struct {
	ctx          context.Context
	pathname     string    // Given name for the underlying file on the filesystem
	lastReadTime time.Time // Last time a log line was read from this file
	llp          logline.Processor

	finishedMu sync.Mutex // protects `finished`
	finished   bool       // The filestream is finished and can no longer be used.
}

// newFileStream creates a new log stream from a regular file.
func newFileStream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, fi os.FileInfo, llp logline.Processor, seekToStart bool) (LogStream, error) {
	fs := &fileStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp}
	if err := fs.stream(ctx, wg, waker, fi, !seekToStart); err != nil {
		return nil, err
	}
	return fs, nil
}

func (fs *fileStream) LastReadTime() time.Time {
	return fs.lastReadTime
}

func (fs *fileStream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, fi os.FileInfo, seekToEnd bool) error {
	fd, err := os.OpenFile(fs.pathname, os.O_RDONLY, 0600)
	if err != nil {
		logErrors.Add(fs.pathname, 1)
		return err
	}
	glog.V(2).Infof("opened new file %v", fd)
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			err := fd.Close()
			if err != nil {
				logErrors.Add(fs.pathname, 1)
				glog.Info(err)
			}
		}()
		if seekToEnd {
			_, err := fd.Seek(0, io.SeekEnd)
			if err != nil {
				logErrors.Add(fs.pathname, 1)
				glog.Info(err)
			}
			glog.Infof("%v: seeked to end", fd)
		}

		b := make([]byte, defaultReadBufferSize)
		partial := bytes.NewBufferString("")
		for {
			// Blocking read but regular files can return EOF straight away.
			count, err := fd.Read(b)
			glog.Infof("%v, read %d bytes, err is %v", fd, count, err)
			// If we have read no bytes and are at EOF, check for truncation and rotation.
			if err == io.EOF && count == 0 {
				glog.Infof("%v, eof an no bytes", fd)
				// Both rotation and truncation need to stat, so check for rotation first.  It is assumed that rotation is the more common change pattern anyway
				newfi, serr := os.Stat(fs.pathname)
				if serr != nil {
					glog.Info(serr)
					if os.IsNotExist(serr) {
						glog.Info("no longer exists, mark as finished")
						// If the file no longer exists, then there's nothing to
						// reopen and thus we must be done here.  The caller may
						// find this file in the future, and it can create a new
						// logstream to handle it.
						if partial.Len() > 0 {
							sendLine(ctx, fs.pathname, partial, fs.llp)
						}
						fs.finishedMu.Lock()
						fs.finished = true
						fs.finishedMu.Unlock()
						return
					}
					logErrors.Add(fs.pathname, 1)
					goto Sleep
				}
				if !os.SameFile(fi, newfi) {
					glog.Infof("%v:adding a new file routine", fd)
					fileRotations.Add(fs.pathname, 1)
					go fs.stream(ctx, wg, waker, newfi, false)
					// We're at EOF so there's nothing left to read here.
					return
				}
				currentOffset, serr := fd.Seek(0, io.SeekCurrent)
				if serr != nil {
					logErrors.Add(fs.pathname, 1)
					glog.Info(serr)
					continue
				}
				// We know that newfi is the same file here.
				if currentOffset != 0 && newfi.Size() < currentOffset {
					glog.Infof("%v: truncate? currentoffset is %d and size is %d", fd, currentOffset, newfi.Size())
					// About to lose all remaining data because of the truncate so flush the accumulator.
					if partial.Len() > 0 {
						sendLine(ctx, fs.pathname, partial, fs.llp)
					}
					p, serr := fd.Seek(0, io.SeekStart)
					if serr != nil {
						logErrors.Add(fs.pathname, 1)
						glog.Info(serr)
					}
					glog.Infof("Seeked to %d", p)
					fileTruncates.Add(fs.pathname, 1)
					continue
				}
			}
			glog.Info("decode and send")
			decodeAndSend(ctx, fs.llp, fs.pathname, count, b[:count], partial)
			if count > 0 {
				fs.lastReadTime = time.Now()
			}
		Sleep:
			// If we have stalled, then pause, otherwise loop back.
			if err == io.EOF || ctx.Err() != nil {
				glog.Info("waiting")
				select {
				case <-ctx.Done():
					if partial.Len() > 0 {
						sendLine(ctx, fs.pathname, partial, fs.llp)
					}
					fs.finishedMu.Lock()
					fs.finished = true
					fs.finishedMu.Unlock()
					return
				case <-waker.Wake():
					// sleep until next Wake()
					glog.Infof("%v: Wake received", fd)
				}
			}
		}
	}()

	return nil
}

func (fs *fileStream) IsFinished() bool {
	fs.finishedMu.Lock()
	defer fs.finishedMu.Unlock()
	return fs.finished
}
