// Copyright 2024 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"io"

	"github.com/google/mtail/internal/logline"
)

// logLines counts the number of lines read per log file.
// var logLines = expvar.NewMap("log_lines_total")

// LineReader reads lines from input and sends lines through the channel
type LineReader struct {
	sourcename string
	lines      chan<- *logline.LogLine
	f          io.Reader
	size       int
	buf        []byte
	off        int // tracks the start of the next line in buf
	end        int // tracks the end of the next line in buf
}

func NewLineReader(sourcename string, lines chan<- *logline.LogLine, f io.Reader, size int) *LineReader {
	return &LineReader{
		sourcename: sourcename,
		lines:      lines,
		f:          f,
		size:       size,
		buf:        make([]byte, 0, size),
	}
}

func (lr *LineReader) ReadAndSend(ctx context.Context) (count int, err error) {
	if cap(lr.buf)-len(lr.buf) < lr.size {
		len := len(lr.buf)
		lr.buf = append(make([]byte, 0, len+lr.size), lr.buf...)

	}
	count, err = lr.f.Read(lr.buf[len(lr.buf):cap(lr.buf)])
	lr.buf = lr.buf[:len(lr.buf)+count] // reslice to set len
	if count > 0 {
		for ok := true; ok; ok = lr.send(ctx) {
		}
		// reslice to drop earlier bytes
		lr.buf = lr.buf[lr.off:len(lr.buf)]
		lr.off = 0
	}
	return
}

func (lr *LineReader) send(ctx context.Context) bool {
	lim := min(len(lr.buf), cap(lr.buf))
	i := bytes.IndexByte(lr.buf[lr.off:lim], '\n')
	// No newlines in the latest bytes, wait for next read
	if i < 0 {
		return false
	}
	end := lr.off + i // excluding delim
	line := string(lr.buf[lr.off:end])
	logLines.Add(lr.sourcename, 1)
	lr.lines <- logline.New(ctx, lr.sourcename, line)
	lr.off = end + 1 // set past delim
	return true
}

func (lr *LineReader) Finish(ctx context.Context) {
	line := string(lr.buf[lr.off:])
	if len(line) == 0 {
		return
	}
	logLines.Add(lr.sourcename, 1)
	lr.lines <- logline.New(ctx, lr.sourcename, line)
}
