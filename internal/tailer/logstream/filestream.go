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

	"github.com/google/mtail/internal/logline"
)

type fileStream struct {
	ctx          context.Context
	pathname     string        // Given name for the underlying named pipe on the filesystem
	lastReadTime time.Time     // Last time a log line was read from this named pipe
	file         *os.File      // The file descriptor of the open pipe
	partial      *bytes.Buffer // Partial line accumulator
	llp          logline.Processor
}

func newFileStream(ctx context.Context, wg *sync.WaitGroup, pathname string, llp logline.Processor) (LogStream, error) {
	fd, err := os.OpenFile(pathname, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}
	fs := &fileStream{ctx: ctx, pathname: pathname, lastReadTime: time.Now(), file: fd, partial: bytes.NewBufferString(""), llp: llp}
	return fs, nil
}

func (fs *fileStream) Reopen(fi *os.FileInfo) error {
	return nil
}

func (fs *fileStream) Close() error {
	return nil
}

func (fs *fileStream) LastReadTime() time.Time {
	return fs.lastReadTime
}

func (fs *fileStream) read() error {
	b := make([]byte, 0, defaultReadBufferSize)
	capB := cap(b)
	totalBytes := 0
	for {
		n, err := fs.file.Read(b[:capB])
		totalBytes += n
		b = b[:n]
		// If we have read no bytes and are at EOF, check for truncation.
		if err == io.EOF && totalBytes == 0 {
			if fs.isTruncated() {
				// If true, we've already seeked back to the start of the file.
				continue
			}
		}
		decodeAndSend(fs.ctx, fs.llp, fs.pathname, n, &b, fs.partial)
		// Return on any error, including EOF.
		if err != nil {
			// Update the last read time if we were able to read anything.
			if totalBytes > 0 {
				fs.lastReadTime = time.Now()
			}
			return err
		}
	}
}

func (fs *fileStream) isTruncated() bool {
	return false
}
