// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"expvar"
	"unicode/utf8"

	"github.com/google/mtail/internal/logline"
)

// lineCount counts the number of lines read per log file
var lineCount = expvar.NewMap("logstream_lines_total")

// decodeAndSend transforms the byte addary `b` into unicode in `partial`, sending to the llp as each newline is decoded.
func decodeAndSend(ctx context.Context, llp logline.Processor, pathname string, n int, b []byte, partial *bytes.Buffer) {
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
			sendLine(ctx, pathname, partial, llp)
		}
	}
}

func sendLine(ctx context.Context, pathname string, partial *bytes.Buffer, llp logline.Processor) {
	llp.ProcessLogLine(ctx, logline.New(ctx, pathname, partial.String()))
	lineCount.Add(pathname, 1)
	partial.Reset()
}
