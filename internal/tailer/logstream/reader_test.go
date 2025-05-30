// Copyright 2024 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestReadAndSend(t *testing.T) {
	for _, tc := range []struct {
		name        string
		input       []byte
		wantOut     []string
		wantPartial string
	}{
		{
			name:        "char",
			input:       []byte("c"),
			wantPartial: "c",
		},
		{
			name:    "newline terminated",
			input:   []byte("line\n"),
			wantOut: []string{"line"},
		},
		{
			name:    "utf replacement char",
			input:   []byte("�\n"),
			wantOut: []string{"�"},
		},
		{
			name:    "non ascii",
			input:   []byte("えむてる\n"),
			wantOut: []string{"えむてる"},
		},
		{
			name:    "two lines",
			input:   []byte("line 1\nline 2\n"),
			wantOut: []string{"line 1", "line 2"},
		},
		{
			name:    "windows line ending",
			input:   []byte("line 1\r\nline \r2\n"),
			wantOut: []string{"line 1", "line \r2"},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			lines := make(chan *logline.LogLine, 5)

			lr := NewLineReader("testPath", lines, bytes.NewReader(tc.input), 4096, nil)

			_, err := lr.ReadAndSend(context.Background())
			if err != nil {
				t.Errorf("ReadAndSend(%v) -> %v", tc.input, err)
			}

			close(lines)
			var gotLines []string
			for _, l := range testutil.LinesReceived(lines) {
				gotLines = append(gotLines, l.Line)
			}

			if diff := cmp.Diff(tc.wantOut, gotLines); diff != "" {
				t.Errorf("ReadAndSend(%v) unexpected lines diff (-want +got):\n%s", tc.input, diff)
			}
			if diff := cmp.Diff(tc.wantPartial, string(lr.buf[lr.off:len(lr.buf)])); diff != "" {
				t.Errorf("ReadAndSend(%v) unexpected partials diff (-want +got):\n%s", tc.input, diff)
			}
		})
	}
}

func TestReadAndSendSplitBytes(t *testing.T) {
	// Make sure that the send works when a rune is split across multiple calls to ReadAndSend

	orig := "えむてる"
	origBytes := []byte(orig + "\n")

	for chunkSize := 1; chunkSize <= len(origBytes); chunkSize++ {
		t.Run(fmt.Sprintf("%d", chunkSize), func(t *testing.T) {
			lines := make(chan *logline.LogLine, 20)

			lr := NewLineReader("testPath", lines, bytes.NewReader(origBytes), chunkSize, nil)

			byteCount := 0
			for chunkOffset := 0; chunkOffset < len(origBytes); chunkOffset += chunkSize {
				chunkEnd := chunkOffset + chunkSize
				if chunkEnd > len(orig) {
					chunkEnd = len(orig)
				}

				count, err := lr.ReadAndSend(context.Background())
				if err != nil && !errors.Is(err, io.EOF) {
					t.Errorf("ReadAndSend(%v) -> %v", origBytes[chunkOffset:chunkEnd], err)
				}
				byteCount += count
			}

			if byteCount != len(origBytes) {
				t.Errorf("decodeAndSend(): bytes read -> %d, want %d", byteCount, len(origBytes))
			}

			close(lines)
			gotLines := testutil.LinesReceived(lines)
			if len(gotLines) != 1 {
				t.Fatalf("decodeAndSend() lines emitted -> %d, want %d", len(gotLines), 1)
			}
			if diff := cmp.Diff(orig, gotLines[0].Line, cmpopts.EquateComparable()); diff != "" {
				t.Errorf("decodeAndSend() line unexpected diff (-want +got):\n%s", diff)
			}
		})
	}
}
