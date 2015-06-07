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

func (t *Tailer) addWatched(path string) {
	t.watchedLock.Lock()
	defer t.watchedLock.Unlock()
	t.watched[path] = struct{}{}
}

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

// TailFile registers a file descriptor to be tailed.
func (t *Tailer) TailFile(f afero.File) error {
	logCount.Add(1)
	return t.startNewFile(f, false)
}

// handleLogUpdate reads all available bytes from an already opened file
// identified by pathname, and sends them to be processed on the lines channel.
func (t *Tailer) handleLogUpdate(pathname string) {
	f, ok := t.files[pathname]
	if !ok {
		glog.Infof("No file found for %q", pathname)
		return
	}
	var err error
	t.partials[pathname], err = t.readFile(f, t.partials[pathname])
	if err != nil {
		glog.Info(err)
	}
}

func (t *Tailer) readFile(f afero.File, partialIn string) (partialOut string, err error) {
	for {
		partialOut, err = t.readPartial(f, partialIn)
		if err != nil {
			if err == io.EOF {
				// end of file for now, return
				return
			}
			glog.Infof("Failed to read updates from %q: %s", f.Name(), err)
			return
		}
		partialIn = partialOut
	}
}

// readPartial reads blocks of 4096 bytes from the File, sending lines to the
// channel as it encounters newlines.  If EOF is encountered, the partial line
// is returned to be concatenated with on the next call.
func (t *Tailer) readPartial(f afero.File, partial string) (string, error) {
	b := make([]byte, 4096)
	n, err := f.Read(b)
	if err != nil {
		return partial, err // TODO(jaq): handle this path better
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
	return partial, nil
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
	if !t.isWatching(pathname) {
		glog.V(1).Info("Not watching path %q, ignoring.\n", pathname)
		return
	}

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
			// Always seek to start on log rotation.
			glog.Infof("Seek to start on %s", pathname)
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

// openLogPath opens a new log file at pathname, and optionally seeks to the
// start or end of the file. Rotated logs should read from the start, but logs
// opened for the first time read from the end.
func (t *Tailer) openLogPath(pathname string, seekStart bool) {
	if !t.isWatching(pathname) {
		glog.Infof("Not watching %q, ignoring.", pathname)
		return
	}

	d := path.Dir(pathname)
	if !t.isWatching(d) {
		err := t.w.Add(d)
		if err != nil {
			glog.Infof("Adding a create watch failed on %q: %s", d, err)
		}
		t.addWatched(d)
	}

	f, err := t.fs.Open(pathname)
	if err != nil {
		// Doesn't exist yet. We're watching the directory, so we'll pick it up
		// again on create; return successfully.
		if os.IsNotExist(err) {
			return
		}
		glog.Infof("Failed to open %q for reading: %s", pathname, err)
		logErrors.Add(pathname, 1)
		return
	}
	err = t.startNewFile(f, seekStart)
	if err != nil {
		glog.Info(err)
	}
}

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
		t.partials[f.Name()], err = t.readFile(f, "")
		if err != nil && err != io.EOF {
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
			t.handleLogUpdate(e.Pathname)
		case watcher.CreateEvent:
			t.handleLogCreate(e.Pathname)
		default:
			glog.Infof("Unexpected event %q", e)
		}
	}
	glog.Infof("Shutting down tailer.")
	close(t.lines)
}

func (t *Tailer) readForever(f afero.File) {
	var err error
	partial := ""
	for {
		partial, err = t.readFile(f, partial)
		if err != nil {
			glog.Infof("%s: %s", err, f.Name())
			return
		}
	}
}

// Close signals termination to the watcher.
func (t *Tailer) Close() {
	t.w.Close()
}
