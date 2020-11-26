// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"io"
	"os"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
)

// fileStream streams log lines from a regular file on the file system.  These
// log files are appended to by another process, and are either rotated or
// truncated by that (or yet another) process.  Rotatoin implies that a new
// inode with the same name has been created, the old file descriptor will be
// valid until EOF at which point it's considered finished.  A truncation means
// the same file descriptor is used but the file offset will be reset to 0.
// The latter is potentially lossy as far as mtail is concerned, if the last
// logs are not read before truncation occurs.  When an EOF is read, the goroutine tests for both truncation and inode change and resets or spins off a new goroutine and closes itself down.
// The shared context is used for cancellation.
type fileStream struct {
	ctx          context.Context
	pathname     string    // Given name for the underlying file on the filesystem
	lastReadTime time.Time // Last time a log line was read from this file
	llp          logline.Processor
	pollChannel  chan struct{}
}

// newFileStream creates a new log stream from a regular file.
func newFileStream(ctx context.Context, wg *sync.WaitGroup, pathname string, fi os.FileInfo, llp logline.Processor) (LogStream, error) {
	fs := &fileStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), llp: llp, pollChannel: make(chan struct{}, 0)}
	wg.Add(1)
	go fs.read(ctx, wg, fi, true)
	return fs, nil
}

func (fs *fileStream) LastReadTime() time.Time {
	return fs.lastReadTime
}

func (fs *fileStream) Poll() {
	fs.pollChannel <- struct{}{}
}

func (fs *fileStream) read(ctx context.Context, wg *sync.WaitGroup, fi os.FileInfo, seekToEnd bool) {
	defer wg.Done()
	fd, err := os.OpenFile(fs.pathname, os.O_RDONLY, 0600)
	if err != nil {
		glog.Info(err)
	}
	glog.Infof("opened new file %v", fd)
	defer func() {
		err := fd.Close()
		if err != nil {
			glog.Info(err)
		}
	}()
	if seekToEnd {
		_, err := fd.Seek(0, io.SeekEnd)
		if err != nil {
			glog.Info(err)
		}
		glog.Infof("%v: seeked to end", fd)
	}

	b := make([]byte, 0, defaultReadBufferSize)
	capB := cap(b)
	partial := bytes.NewBufferString("")
	for {
		// Blocking read but regular files can return EOF straight away.
		n, err := fd.Read(b[:capB])
		glog.Infof("%v, read %d bytes, err is %v", fd, n, err)
		b = b[:n]
		// If we have read no bytes and are at EOF, check for truncation and rotation.
		if err == io.EOF && n == 0 {
			glog.Infof("%v, eof an no bytes", fd)
			// Both rotation and truncation need to stat, so check for rotation first.  It is assumed that rotation is the more common change pattern anyway
			newfi, err := os.Stat(fs.pathname)
			if err != nil {
				glog.Info(err)
				continue
			}
			if !os.SameFile(fi, newfi) {
				glog.Infof("%v:adding a new file routine", fd)
				wg.Add(1)
				go fs.read(ctx, wg, newfi, false)
				// We're at EOF so there's nothing left to read here.
				return
			}
			currentOffset, err := fd.Seek(0, io.SeekCurrent)
			if err != nil {
				glog.Info(err)
				continue
			}
			// We know that newfi is the same file here.
			if currentOffset != 0 && newfi.Size() < currentOffset {
				glog.Infof("%v: truncate? currentoffset is %d and size is %d", fd, currentOffset, newfi.Size())
				// About to lose all remaining data because of the truncate so flush the accumulator.
				if partial.Len() > 0 {
					sendLine(ctx, fs.pathname, partial, fs.llp)
				}
				_, err := fd.Seek(0, io.SeekStart)
				if err != nil {
					glog.Info(err)
				}
				continue
			}
		}
		decodeAndSend(ctx, fs.llp, fs.pathname, n, &b, partial)
		// If we have stalled, then pause, otherwise loop back.
		if err == io.EOF || ctx.Err() != nil {
			// Update the last read time if we were able to read anything.
			if n > 0 {
				fs.lastReadTime = time.Now()
			}
			select {
			case e := <-fs.pollChannel:
				// sleep until next Poll()
				glog.Infof("%v: Poll received", fd)
				select {
				case fs.pollChannel <- e:
					glog.Infof("%v: resending poll", fd)
					// send to next listener if any
				default:
				}
			case <-ctx.Done():
				if partial.Len() > 0 {
					sendLine(ctx, fs.pathname, partial, fs.llp)
				}
				return
			}
		}
	}
}

func (fs *fileStream) isTruncated() bool {
	return false
}
