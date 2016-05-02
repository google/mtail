// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package tailer provides a class that is responsible for tailing log files
// and extracting new log lines to be passed into the virtual machines.
package tailer

// For regular files, mtail gets notified on modifications (i.e. appends) to
// log files that are being watched, in order to read the new lines. Log files
// can also be rotated, so mtail is also notified of creates in the log file
// directory.

import (
	"errors"
	"expvar"
	"fmt"
	"io"
	"os"
	"path"
	"path/filepath"
	"sync"
	"syscall"
	"time"
	"unicode/utf8"

	"github.com/golang/glog"

	"github.com/google/mtail/watcher"

	"github.com/spf13/afero"
)

var (
	logCount     = expvar.NewInt("log_count")
	logErrors    = expvar.NewMap("log_errors_total")
	logRotations = expvar.NewMap("log_rotations_total")
)

// Tailer receives notification of changes from a Watcher and extracts new log
// lines from files. It also handles new log file creation events and log
// rotations.
type Tailer struct {
	w watcher.Watcher

	watched     map[string]struct{}   // Names of logs being watched.
	watchedLock sync.RWMutex          // protects `watched'
	lines       chan<- string         // Logfile lines being emitted.
	files       map[string]afero.File // File handles for each pathname.
	filesLock   sync.Mutex            // protects `files'
	partials    map[string]string     // Accumulator for the currently read line for each pathname.

	shutdown bool

	fs afero.Fs // mockable filesystem interface
}

// Options configures a Tailer
type Options struct {
	Lines chan<- string
	W     watcher.Watcher // Not required, will use watcher.LogWatcher if it is zero.
	FS    afero.Fs        // Not required, will use afero.OsFs if it is zero.
}

// New returns a new Tailer, configured with the supplied Options
func New(o Options) (*Tailer, error) {
	if o.Lines == nil {
		return nil, errors.New("tailer needs lines")
	}
	fs := o.FS
	if fs == nil {
		fs = &afero.OsFs{}
	}
	w := o.W
	if w == nil {
		var err error
		w, err = watcher.NewLogWatcher()
		if err != nil {
			return nil, fmt.Errorf("Couldn't create a watcher for tailer: %s", err)
		}
	}
	t := &Tailer{
		w:        w,
		watched:  make(map[string]struct{}),
		lines:    o.Lines,
		files:    make(map[string]afero.File),
		partials: make(map[string]string),
		fs:       fs,
	}
	go t.run()
	return t, nil
}

// addWatched adds a path to the list of watched items.
func (t *Tailer) addWatched(path string) {
	t.watchedLock.Lock()
	defer t.watchedLock.Unlock()
	t.watched[path] = struct{}{}
}

// isWatching indicates if the path is being watched. It includes both
// filenames and directories.
func (t *Tailer) isWatching(path string) bool {
	t.watchedLock.RLock()
	defer t.watchedLock.RUnlock()
	_, ok := t.watched[path]
	return ok
}

// Tail registers a file path to be tailed.
func (t *Tailer) Tail(pathname string) {
	fullpath, err := filepath.Abs(pathname)
	if err != nil {
		glog.Infof("Failed to find absolute path for %q: %s\n", pathname, err)
		return
	}
	if !t.isWatching(fullpath) {
		t.addWatched(fullpath)
		logCount.Add(1)
		t.openLogPath(fullpath, false)
	}
}

// TailFile registers a file handle to be tailed.  There is no filesystem to
// watch, so no watches are registered, and no file paths are opened.
func (t *Tailer) TailFile(f afero.File) error {
	logCount.Add(1)
	return t.startNewFile(f, false)
}

// handleLogUpdate reads all available bytes from an already opened file
// identified by pathname, and sends them to be processed on the lines channel.
func (t *Tailer) handleLogUpdate(pathname string) {
	t.filesLock.Lock()
	fd, ok := t.files[pathname]
	t.filesLock.Unlock()
	if !ok {
		glog.Warningf("No file descriptor found for %q, but is being watched", pathname)
		return
	}
	var err error
	t.partials[pathname], err = t.read(fd, t.partials[pathname])
	if err != nil && err != io.EOF {
		glog.Info(err)
	}
}

// read reads blocks of 4096 bytes from the File, sending lines to the
// channel as it encounters newlines.  If EOF is encountered, the partial line
// is returned to be concatenated with on the next call.
func (t *Tailer) read(f afero.File, partialIn string) (partialOut string, err error) {
	partial := partialIn
	b := make([]byte, 0, 4096)
	for !t.shutdown {
		n, err := f.Read(b[:cap(b)])
		b = b[:n]
		if err != nil {
			return partial, err
		}

		for i, width := 0, 0; i < len(b) && i < n; i += width {
			var rune rune
			rune, width = utf8.DecodeRune(b[i:])
			switch {
			case rune != '\n':
				partial += string(rune)
			default:
				// send off line for processing
				t.lines <- partial
				// reset accumulator
				partial = ""
			}
		}
	}
	return partial, fmt.Errorf("reader shutdown requested")
}

// inode returns the inode number of a file, or 0 if the file has no underlying Sys implementation.
func inode(f os.FileInfo) uint64 {
	s := f.Sys()
	if s == nil {
		return 0
	}
	switch s := s.(type) {
	case *syscall.Stat_t:
		return s.Ino
	default:
		return 0
	}
}

// handleLogCreate handles both new and rotated log files.
func (t *Tailer) handleLogCreate(pathname string) {
	t.filesLock.Lock()
	fd, ok := t.files[pathname]
	t.filesLock.Unlock()
	if ok {
		s1, err := fd.Stat()
		if err != nil {
			glog.Infof("Stat failed on %q: %s", t.files[pathname].Name(), err)
			return
		}
		s2, err := t.fs.Stat(pathname)
		if err != nil {
			glog.Infof("Stat failed on %q: %s", pathname, err)
			return
		}
		if inode(s1) != inode(s2) {
			logRotations.Add(pathname, 1)
			// flush the old log, pathname is still an index into t.files with the old inode.
			t.handleLogUpdate(pathname)
			fd.Close()
			err := t.w.Remove(pathname)
			if err != nil {
				glog.Info("Failed removing watches on", pathname)
			}
			t.openLogPath(pathname, true)
		} else {
			glog.Infof("Path %s already being watched, and inode not changed.",
				pathname)
		}
	} else {
		// Freshly opened log file, never seen before.
		t.openLogPath(pathname, true)
	}
}

// openLogPath opens a log file named by pathname.
func (t *Tailer) openLogPath(pathname string, seenBefore bool) {
	d := path.Dir(pathname)
	if !t.isWatching(d) {
		err := t.w.Add(d)
		if err != nil {
			glog.Infof("Adding a create watch failed on %q: %s", d, err)
		}
		t.addWatched(d)
	}

	retries := 3
	retryDelay := 1 * time.Millisecond
	var f afero.File
	var err error
	for retries > 0 {
		f, err = t.fs.Open(pathname)
		if err == nil {
			break
		}
		// Doesn't exist yet. We're watching the directory, so we'll pick it up
		// again on create; return successfully.
		if os.IsNotExist(err) {
			return
		}
		glog.Infof("Failed to open %q for reading: %s", pathname, err)
		logErrors.Add(pathname, 1)
		// seenBefore indicates also that we're rotating a file that previously worked, so retry.
		if seenBefore {
			retries = retries - 1
			time.Sleep(retryDelay)
			retryDelay = retryDelay + retryDelay
		} else {
			return
		}
	}
	err = t.startNewFile(f, seenBefore)
	if err != nil {
		glog.Error(err)
	}
}

// startNewFile optionally seeks to the start or end of the file, then starts
// the consumption of log lines. Rotated logs should read from the start, but
// logs opened for the first time read from the end.
func (t *Tailer) startNewFile(f afero.File, seekStart bool) error {
	fi, err := f.Stat()
	if err != nil {
		// Stat failed, log error and return.
		logErrors.Add(f.Name(), 1)
		return fmt.Errorf("Failed to stat %q: %s", f.Name(), err)
	}
	switch m := fi.Mode(); {
	case m&os.ModeType == 0:
		if seekStart {
			f.Seek(0, os.SEEK_SET)
		} else {
			f.Seek(0, os.SEEK_END)
		}
		err = t.w.Add(f.Name())
		if err != nil {
			return fmt.Errorf("Adding a change watch failed on %q: %s", f.Name(), err)
		}
		// In case the new log has been written to already, attempt to read the
		// first lines.
		t.partials[f.Name()], err = t.read(f, "")
		if err != nil {
			if err == io.EOF {
				// Don't worry about EOF on first read, that's expected.
				break
			}
			return err
		}
	case m&os.ModeType == os.ModeNamedPipe:
		go t.readForever(f)
	default:
		return fmt.Errorf("Can't open files with mode %v: %s", m&os.ModeType, f.Name())
	}
	t.filesLock.Lock()
	t.files[f.Name()] = f
	t.filesLock.Unlock()
	glog.Infof("Tailing %s", f.Name())

	return nil
}

// start is the main event loop for the Tailer.
// It receives notification of log file changes from the watcher channel, and
// handles them.
func (t *Tailer) run() {
	for e := range t.w.Events() {
		switch e := e.(type) {
		case watcher.UpdateEvent:
			if t.isWatching(e.Pathname) {
				t.handleLogUpdate(e.Pathname)
			}
		case watcher.CreateEvent:
			if t.isWatching(e.Pathname) {
				t.handleLogCreate(e.Pathname)
			}
		case watcher.DeleteEvent:
		default:
			glog.Infof("Unexpected event %#v", e)
		}
	}
	glog.Infof("Shutting down tailer.")
	close(t.lines)
}

// readForever handles non-logfile inputs by reading from the File until it is closed.
func (t *Tailer) readForever(f afero.File) {
	var err error
	partial := ""
	for !t.shutdown {
		partial, err = t.read(f, partial)
		// We want to exit at EOF, because the FD has been closed.
		if err != nil {
			glog.Infof("%s: %s", err, f.Name())
			return
		}
	}
}

// Close signals termination to the watcher.
func (t *Tailer) Close() {
	t.shutdown = true
	t.w.Close()
}
