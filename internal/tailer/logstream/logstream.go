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
	"expvar"
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

var (
	// logErrors counts the IO errors encountered per log
	logErrors = expvar.NewMap("log_errors_total")
)

// LogStream
type LogStream interface {
	LastReadTime() time.Time // Return the time when the last log line was read from the source
	IsComplete() bool        // True if the logstream has completed work and cannot recover.  The caller should clean up this logstream, creating a new logstream on a pathname if necessary.
}

// defaultReadTimeout contains the timeout for reads from nonblocking read sources.
const defaultReadTimeout = 10 * time.Millisecond

// defaultReadBufferSize the size of the buffer for reading bytes into
const defaultReadBufferSize = 4096

// New creates a LogStream from the file object located at the absolute path
// `pathname`.  The LogStream will watch `ctx` for a cancellation signal, and
// notify the `wg` when it is Done.  Log lines will be sent to the `llp` per
// the `logline.Processor` interface specification.  `seekToStart` is only used
// for testing and only works for regular files that can be seeked.
func New(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, llp logline.Processor, seekToStart bool) (LogStream, error) {
	fi, err := os.Stat(pathname)
	if err != nil {
		logErrors.Add(pathname, 1)
		return nil, err
	}
	switch m := fi.Mode(); {
	case m.IsRegular():
		return newFileStream(ctx, wg, waker, pathname, fi, llp, seekToStart)
	case m&os.ModeType == os.ModeNamedPipe:
		return newPipeStream(ctx, wg, waker, pathname, fi, llp)
	case m&os.ModeType == os.ModeSocket:
		return newSocketStream(ctx, wg, waker, pathname, fi, llp)
	default:
		return nil, fmt.Errorf("unsupported file object type at %q", pathname)
	}
}
