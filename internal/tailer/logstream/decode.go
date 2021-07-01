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
func decodeAndSend(ctx context.Context, lines chan<- *logline.LogLine, pathname string, n int, b []byte, partial *bytes.Buffer) {
	var (
		rune  rune
		width int
	)
	for i := 0; i < len(b) && i < n; i += width {
		rune, width = utf8.DecodeRune(b[i:])
		// Most file-based log sources will end with \n on Unixlike systems.
		// On Windows they appear to be both \r\n.  syslog disallows \r (and \t
		// and others) and writes them escaped, per syslog(7).  [RFC
		// 3164](https://www.ietf.org/rfc/rfc3164.txt) disallows newlines in
		// the message: "The MSG part of the syslog packet MUST contain visible
		// (printing) characters."  So for now let's assume that a \r only
		// occurs at the end of a line anyway, and we can just eat it.
		switch {
		case rune == '\r':
			// nom
		case rune == '\n':
			sendLine(ctx, pathname, partial, lines)
		default:
			partial.WriteRune(rune)
		}
	}
}

func sendLine(ctx context.Context, pathname string, partial *bytes.Buffer, lines chan<- *logline.LogLine) {
	glog.V(2).Infof("sendline")
	logLines.Add(pathname, 1)
	lines <- logline.New(ctx, pathname, partial.String())
	partial.Reset()
}
