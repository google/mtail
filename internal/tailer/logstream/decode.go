// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"expvar"
	"unicode/utf8"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
)

// logLines counts the number of lines read per log file.
var logLines = expvar.NewMap("log_lines_total")

// decodeAndSend transforms the byte array `b` into unicode in `partial`, sending to the llp as each newline is decoded.
func (s *streamBase) decodeAndSend(ctx context.Context, lines chan<- *logline.LogLine, pathname string, n int, b []byte, partial *bytes.Buffer) int {
	var (
		r     rune
		width int
		count int
	)
	var i int
	for ; i < len(b) && i < n; i += width {
		r, width = utf8.DecodeRune(b[i:])
		if r == utf8.RuneError {
			if len(b)-i > 10 {
				// If there are more than enough bytes in the buffer
				// after this, ignore the error as it's not fixable.
				// If not, return so that the caller can try again
				// with a larger buffer, in the event that a unicode
				// character has been cut in half by the buffer.

				count += width
				continue
			}
			return count
		}
		// Most file-based log sources will end with \n on Unixlike systems.
		// On Windows they appear to be both \r\n.  syslog disallows \r (and \t
		// and others) and writes them escaped, per syslog(7).  [RFC
		// 3164](https://www.ietf.org/rfc/rfc3164.txt) disallows newlines in
		// the message: "The MSG part of the syslog packet MUST contain visible
		// (printing) characters."  So for now let's assume that a \r only
		// occurs at the end of a line anyway, and we can just eat it.
		switch {
		case r == '\r':
			// nom
		case r == '\n':
			s.sendLine(ctx, pathname, partial, lines)
		default:
			partial.WriteRune(r)
		}

		count += width
	}
	return count
}

func (s *streamBase) sendLine(ctx context.Context, pathname string, partial *bytes.Buffer, lines chan<- *logline.LogLine) {
	glog.V(2).Infof("sendline")
	logLines.Add(pathname, 1)
	lines <- logline.New(ctx, pathname, partial.String())
	partial.Reset()
}
