// Copyright 2024 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"expvar"
	"io"
	"time"

	"github.com/jaqx0r/mtail/internal/logline"
)

// logLines counts the number of lines read per log file.
var logLines = expvar.NewMap("log_lines_total")

// LineReader reads lines from input and sends lines through the channel
type LineReader struct {
	sourcename string                  // name of owner, for sending loglines
	lines      chan<- *logline.LogLine // not owned
	f          io.Reader               // not owned
	cancel     context.CancelFunc
	staleTimer *time.Timer // call CancelFunc if no read in 24h

	size int
	buf  []byte
	off  int // tracks the start of the next line in buf
}

// NewLineReader creates a new LineReader
func NewLineReader(sourcename string, lines chan<- *logline.LogLine, f io.Reader, size int, cancel context.CancelFunc) *LineReader {
	return &LineReader{
		sourcename: sourcename,
		lines:      lines,
		f:          f,
		cancel:     cancel,
		size:       size,
		buf:        make([]byte, 0, size),
	}
}

// ReadAndSend reads bytes from f, attempts to find line endings in the bytes read, and sends them to the lines channel.  It manages the read buffer size to make sure we can always read size bytes.
func (lr *LineReader) ReadAndSend(ctx context.Context) (count int, err error) {
	if cap(lr.buf)-len(lr.buf) < lr.size {
		lr.buf = append(make([]byte, 0, len(lr.buf)+lr.size), lr.buf...)
	}
	count, err = lr.f.Read(lr.buf[len(lr.buf):cap(lr.buf)])
	if lr.staleTimer != nil {
		lr.staleTimer.Stop()
	}
	lr.buf = lr.buf[:len(lr.buf)+count] // reslice to set len
	if count > 0 {
		lr.staleTimer = time.AfterFunc(time.Hour*24, lr.cancel)
		ok := true
		for ok {
			ok = lr.send(ctx)
		}
		// reslice to drop earlier bytes
		lr.buf = lr.buf[lr.off:len(lr.buf)]
		lr.off = 0
	}
	return
}

// send sends the line and resets the buffer offset
func (lr *LineReader) send(ctx context.Context) bool {
	lim := min(len(lr.buf), cap(lr.buf))
	i := bytes.IndexByte(lr.buf[lr.off:lim], '\n')
	// No newlines in the latest bytes, wait for next read
	if i < 0 {
		return false
	}
	end := lr.off + i // excluding delim
	skip := 1         // len of delim char

	// Most file-based log sources will end with \n on Unixlike systems.  On
	// Windows they appear to be both \r\n.  syslog disallows \r (and \t and
	// others) and writes them escaped, per syslog(7).  [RFC
	// 3164](https://www.ietf.org/rfc/rfc3164.txt) disallows newlines in the
	// message: "The MSG part of the syslog packet MUST contain visible
	// (printing) characters."  Thus if the previous char was a \r then ignore
	// it as well.
	if end > 0 && lr.buf[end-1] == '\r' {
		end--
		skip = 2
	}

	line := string(lr.buf[lr.off:end])
	logLines.Add(lr.sourcename, 1)
	lr.lines <- logline.New(ctx, lr.sourcename, line)
	lr.off = end + skip // move past delim
	return true
}

// Finish sends the current accumulated line to the end of the buffer, despite
// there being no closing newline.
func (lr *LineReader) Finish(ctx context.Context) {
	line := string(lr.buf[lr.off:])
	if len(line) == 0 {
		return
	}
	logLines.Add(lr.sourcename, 1)
	lr.lines <- logline.New(ctx, lr.sourcename, line)
}
