// Copyright 2019 Google, Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/golang/glog"

	"github.com/google/mtail/internal/logline"
)

// Log abstracts over different log sources readable by `mtail'.
type Log interface {
	Follow(context.Context) error // Follow a log regardless of renames until EOF or error.
	Read(context.Context) error   // Read bytes from the log source and send to processor
	Close(context.Context) error  // Close the log
	LastReadTime() time.Time      // Return the time when the last bytes were read from the source
	Name() string                 // Return the user-provided name of the log source.
	Pathname() string             // Return the filesystem full pathname of the log source.
}

// NewLog returns an implementation of the Log interface that handles the given
// pathname.  `llp' is a logline.Processor that receives the bytes when read by
// Read().  `seekToStart' indicates that the log should be read from the
// beginning if possible, for files opened when in OneShot mode.
func NewLog(pathname string, llp logline.Processor, seekToStart bool) (Log, error) {
	glog.V(2).Infof("tailer.NewLog(%s, %v)", pathname, seekToStart)
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		return nil, err
	}
	fi, err := os.Stat(absPath)
	if err != nil {
		return nil, err
	}
	switch m := fi.Mode(); {
	case m.IsRegular() || m&os.ModeType == os.ModeNamedPipe:
		return NewFile(pathname, absPath, llp, seekToStart)
	case m&os.ModeType == os.ModeSocket:
		if seekToStart {
			glog.V(2).Infof("ignoring seekToStart=%v as %q is a socket", seekToStart, absPath)
		}
		return NewSocket(pathname, absPath, llp)
	default:
		return nil, fmt.Errorf("don't know how to open %q", absPath)
	}
}
