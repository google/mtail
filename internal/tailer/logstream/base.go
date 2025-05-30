// Copyright 2024 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream

import (
	"github.com/jaqx0r/mtail/internal/logline"
)

type streamBase struct {
	sourcename string // human readable name of the logstream source

	lines chan *logline.LogLine // outbound channel for lines
}

// Lines returns the output log line channel for this stream.  The stream is
// completed when this channel closes.
func (s *streamBase) Lines() <-chan *logline.LogLine {
	return s.lines
}
