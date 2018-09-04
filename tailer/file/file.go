// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package file provides an abstraction over files and named pipes being tailed
// by `mtail`.
package file

import (
	"bytes"
	"expvar"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"
	"unicode/utf8"

	"github.com/golang/glog"
	"github.com/google/mtail/logline"
	"github.com/pkg/errors"
	"github.com/spf13/afero"
)

var (
	// logErrors counts the number of IO errors per log file
	logErrors = expvar.NewMap("log_errors_total")
	// logRotations counts the number of rotations per log file
	logRotations = expvar.NewMap("log_rotations_total")
	// lineCount counts the numbre of lines read per log file
	lineCount = expvar.NewMap("log_lines_total")
	// logTruncs counts the number of log truncation events per log
	logTruncs = expvar.NewMap("log_truncates_total")
)

// File contains the state for a tailed file.
type File struct {
	Pathname string
	file     afero.File
	partial  *bytes.Buffer
}

// New returns a new File named by the given pathname.  seenBefore indicates
// thta mtail believes it's seen this pathname nbefore, and seekToStart
// indicates that the file should be tailed from offset 0, not EOF.
func New(fs afero.Fs, pathname string, seenBefore, seekToStart bool) (*File, error) {
	glog.V(2).Infof("file.New(%s, %v, %v)", pathname, seenBefore, seekToStart)
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		return nil, err
	}
	retries := 3
	retryDelay := 1 * time.Millisecond
	shouldRetry := func() bool {
		// seenBefore indicates also that we're rotating a file that previously worked, so retry.
		if !seenBefore {
			return false
		}
		return retries > 0
	}
	var f afero.File
Retry:
	f, err = fs.Open(absPath)
	if err != nil {
		logErrors.Add(absPath, 1)
		if shouldRetry() {
			retries = retries - 1
			time.Sleep(retryDelay)
			retryDelay = retryDelay + retryDelay
			goto Retry
		}
	}
	if err != nil {
		glog.Infof("openLogPath failed all retries")
		return nil, err
	}
	glog.V(2).Infof("open succeeded %s", absPath)
	fi, err := f.Stat()
	if err != nil {
		// Stat failed, log error and return.
		logErrors.Add(absPath, 1)
		return nil, errors.Wrapf(err, "Failed to stat %q: %s", absPath)
	}
	switch m := fi.Mode(); {
	case m.IsRegular():
		seekWhence := io.SeekEnd
		if seekToStart {
			seekWhence = io.SeekCurrent
		}
		if _, err := f.Seek(0, seekWhence); err != nil {
			return nil, errors.Wrapf(err, "Seek failed on %q", absPath)
		}
		// Named pipes are the same as far as we're concerned, but we can't seek them.
		fallthrough
	case m&os.ModeType == os.ModeNamedPipe:
	default:
		return nil, errors.Errorf("Can't open files with mode %v: %s", m&os.ModeType, absPath)
	}
	return &File{absPath, f, bytes.NewBufferString("")}, nil
}

// Read blocks of 4096 butes from the File, sending LogLines to the given
// channel as newlines are encountered.  If EOF is read, the partial line is
// stored to be concatenated to on the next call.
func (f *File) Read(lines chan<- *logline.LogLine) error {
	b := make([]byte, 0, 4096)
	totalBytes := 0
	for {
		n, err := f.file.Read(b[:cap(b)])
		glog.V(2).Infof("Read count %v err %v", n, err)
		totalBytes += n
		b = b[:n]

		if err == io.EOF && totalBytes == 0 {
			glog.V(2).Info("Suspected truncation.")
			// If there was nothing to be read, perhaps the file just got truncated.
			terr := f.checkForTruncate()
			glog.V(2).Infof("checkForTruncate returned with error '%v'", terr)
			if terr == nil {
				// Try again: offset was greater than filesize and now we've seeked to start.
				continue
			}
		}

		if err != nil {
			return err
		}

		var (
			rune  rune
			width int
		)
		for i := 0; i < len(b) && i < n; i += width {
			rune, width = utf8.DecodeRune(b[i:])
			switch {
			case rune != '\n':
				f.partial.WriteRune(rune)
			default:
				// send off line for processing, blocks if not ready
				// f.Name)( is buggy when using afero memory filesystem
				lines <- logline.NewLogLine(f.Pathname, f.partial.String())
				lineCount.Add(f.Pathname, 1)
				// reset accumulator
				f.partial.Reset()
			}
		}
	}
}

// checkForTruncate checks to see if the current offset into the file
// is past the end of the file based on its size, and if so seeks to
// the start again.  Returns nil iff that happened.
func (f *File) checkForTruncate() error {
	currentOffset, err := f.file.Seek(0, io.SeekCurrent)
	glog.V(2).Infof("current seek position at %d", currentOffset)
	if err != nil {
		return err
	}

	fi, err := f.file.Stat()
	if err != nil {
		return err
	}

	glog.V(2).Infof("File size is %d", fi.Size())
	if currentOffset == 0 || fi.Size() >= currentOffset {
		return fmt.Errorf("no truncate appears to have occurred")
	}

	p, serr := f.file.Seek(0, io.SeekStart)
	glog.V(2).Infof("Truncated?  Seeked to %d: %v", p, serr)
	logTruncs.Add(f.Pathname, 1)
	return serr
}

func (f *File) Stat() (os.FileInfo, error) {
	return f.file.Stat()
}

func (f *File) Close() error {
	return f.file.Close()
}
