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

// logLines counts the number of lines read per log file
var logLines = expvar.NewMap("log_lines_total")

// decodeAndSend transforms the byte addary `b` into unicode in `partial`, sending to the llp as each newline is decoded.
func decodeAndSend(ctx context.Context, lines chan<- *logline.LogLine, pathname string, n int, b []byte, partial *bytes.Buffer) {
	var (
		rune  rune
		width int
	)
	for i := 0; i < len(b) && i < n; i += width {
		rune, width = utf8.DecodeRune(b[i:])
		switch {
		case rune != '\n':
			partial.WriteRune(rune)
		default:
			sendLine(ctx, pathname, partial, lines)
		}
	}
}

func sendLine(ctx context.Context, pathname string, partial *bytes.Buffer, lines chan<- *logline.LogLine) {
	glog.V(2).Infof("sendline")
	logLines.Add(pathname, 1)
	lines <- logline.New(ctx, pathname, partial.String())
	partial.Reset()
}
