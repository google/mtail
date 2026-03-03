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
	"errors"
	"expvar"
	"fmt"
	"net/url"
	"os"
	"sync"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

var (
	// logErrors counts the IO errors encountered per log.
	logErrors = expvar.NewMap("log_errors_total")
	// logOpens counts the opens of new log file descriptors/sockets.
	logOpens = expvar.NewMap("log_opens_total")
	// logCloses counts the closes of old log file descriptors/sockets.
	logCloses = expvar.NewMap("log_closes_total")
)

// LogStream.
type LogStream interface {
	Lines() <-chan *logline.LogLine // Returns the output channel of this LogStream.
}

// defaultReadBufferSize the size of the buffer for reading bytes for files.
//
// Anecdotally the maximum file read buffer is 4GiB, but thats way too massive.
const defaultReadBufferSize = 131072

const stdinPattern = "-"

var (
	ErrUnsupportedURLScheme = errors.New("unsupported URL scheme")
	ErrUnsupportedFileType  = errors.New("unsupported file type")
	ErrEmptySocketAddress   = errors.New("socket address cannot be empty, please provide a unix domain socket filename or host:port")
	ErrNeedsWaitgroup       = errors.New("logstream needs a waitgroup")
)

type OneShotMode bool

const (
	OneShotDisabled OneShotMode = false
	OneShotEnabled  OneShotMode = true
)

// New creates a LogStream from the file object located at the absolute path
// `pathname`.  The LogStream will watch `ctx` for a cancellation signal, and
// notify the `wg` when it is Done.  `oneShot` is used for testing and only
// works for regular files that can be seeked.
func New(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, pathname string, oneShot OneShotMode) (LogStream, error) {
	if wg == nil {
		return nil, ErrNeedsWaitgroup
	}
	u, err := url.Parse(pathname)
	if err != nil {
		return nil, err
	}
	glog.Infof("Parsed url as %v", u)

	path := pathname
	switch u.Scheme {
	default:
		glog.V(2).Infof("%v: %q in path pattern %q, treating as path", ErrUnsupportedURLScheme, u.Scheme, pathname)
	case "unixgram":
		return newDgramStream(ctx, wg, waker, u.Scheme, u.Path, oneShot)
	case "unix":
		return newSocketStream(ctx, wg, waker, u.Scheme, u.Path, oneShot)
	case "tcp":
		return newSocketStream(ctx, wg, waker, u.Scheme, u.Host, oneShot)
	case "udp":
		return newDgramStream(ctx, wg, waker, u.Scheme, u.Host, oneShot)
	case KafkaScheme:
		return newKafkaStream(ctx, wg, u, oneShot)
	case AWS3Scheme:
		return newAWSS3Stream(ctx, wg, waker, u, oneShot)
	case "", "file":
		path = u.Path
	}
	if IsStdinPattern(path) {
		fi, err := os.Stdin.Stat()
		if err != nil {
			logErrors.Add(path, 1)
			return nil, err
		}
		return newFifoStream(ctx, wg, waker, path, fi)
	}
	fi, err := os.Stat(path)
	if err != nil {
		logErrors.Add(path, 1)
		return nil, err
	}
	switch m := fi.Mode(); {
	case m.IsRegular():
		return newFileStream(ctx, wg, waker, path, fi, oneShot)
	case m&os.ModeType == os.ModeNamedPipe:
		return newFifoStream(ctx, wg, waker, path, fi)
	// TODO(jaq): in order to listen on an existing socket filepath, we must unlink and recreate it
	// case m&os.ModeType == os.ModeSocket:
	// 	return newSocketStream(ctx, wg, waker, pathname)
	default:
		return nil, fmt.Errorf("%w: %q", ErrUnsupportedFileType, pathname)
	}
}

func IsStdinPattern(pattern string) bool {
	if pattern == stdinPattern {
		return true
	}
	if pattern == "/dev/stdin" {
		return true
	}
	return false
}
