// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package logstream provides an interface and implementations of log source
// streaming. Each log streaming implementation provides an abstraction that
// makes one pathname look like one perpetual source of logs, even though the
// underlying file objects might be truncated or rotated, or in the case of
// pipes have different open/close semantics.
package logstream

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/google/mtail/internal/logline"
)

// LogStream
type LogStream interface {
	LastReadTime() time.Time // Return the time when the last log line was read from the source
}

// defaultReadTimeout contains the timeout for reads from nonblocking read sources.
const defaultReadTimeout = 10 * time.Millisecond

// defaultReadBufferSize the size of the buffer for reading bytes into
const defaultReadBufferSize = 4096

// New creates a LogStream from the file object located at the absolute path
// `pathname`.  The LogStream will watch `ctx` for a cancellation signal, and
// notify the `wg` when it is Done.  Log lines will be sent to the `llp` per
// the `logline.Processor` interface specification.
func New(ctx context.Context, wg *sync.WaitGroup, pathname string, llp logline.Processor, pollInterval time.Duration) (LogStream, error) {
	fi, err := os.Stat(pathname)
	if err != nil {
		return nil, err
	}
	switch m := fi.Mode(); {
	case m.IsRegular():
		return newFileStream(ctx, wg, pathname, fi, llp, pollInterval)
	case m&os.ModeType == os.ModeNamedPipe:
		return newPipeStream(ctx, wg, pathname, fi, llp, pollInterval)
	case m&os.ModeType == os.ModeSocket:
		return newSocketStream(ctx, wg, pathname, fi, llp, pollInterval)
	default:
		return nil, fmt.Errorf("unsupported file object type at %q", pathname)
	}
}
