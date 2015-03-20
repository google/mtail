// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// tail is responsible for tailing a log file and extracting new log lines to
// be passed into the virtual machines.

// mtail gets notified on modifications (i.e. appends) to log files that are
// being watched, in order to read the new lines. Log files can also be
// rotated, so mtail is also notified of creates in the log file directory.

package main

import (
	"expvar"
	"io"
	"os"
	"path"
	"path/filepath"
	"sync"
	"syscall"
	"unicode/utf8"

	"github.com/golang/glog"
	"github.com/google/mtail/watcher"
)

var (
	log_count     = expvar.NewInt("log_count")
	log_errors    = expvar.NewMap("log_errors_total")
	log_rotations = expvar.NewMap("log_rotations_total")
)

// tailer receives notification of changes from inotify and extracts new log
// lines from files. It also handles new log file creation events and log
// rotations.
type tailer struct {
	w watcher.Watcher

	quit chan bool

	watched      map[string]struct{} // Names of logs being watched.
	watched_lock sync.RWMutex        // protects `watched'
	lines        chan string         // Logfile lines being emitted.
	files        map[string]*os.File // File handles for each pathname.
	files_lock   sync.Mutex          // protects `files'
	partials     map[string]string   // Accumulator for the currently read line for each pathname.
}

// NewTailer returns a new tailer.
func NewTailer(lines chan string, w watcher.Watcher) *tailer {
	t := &tailer{
		w:        w,
		quit:     make(chan bool, 1),
		watched:  make(map[string]struct{}),
		lines:    lines,
		files:    make(map[string]*os.File),
		partials: make(map[string]string),
	}
	go t.start()
	return t
}

func (t *tailer) addWatched(path string) {
	t.watched_lock.Lock()
	defer t.watched_lock.Unlock()
	t.watched[path] = struct{}{}
}

func (t *tailer) isWatching(path string) bool {
	t.watched_lock.RLock()
	defer t.watched_lock.RUnlock()
	_, ok := t.watched[path]
	return ok
}

// Tail registers a file to be tailed.
func (t *tailer) Tail(pathname string) {
	fullpath, err := filepath.Abs(pathname)
	if err != nil {
		glog.Infof("Failed to find absolute path for %q: %s\n", pathname, err)
		return
	}
	if !t.isWatching(fullpath) {
		t.addWatched(fullpath)
		log_count.Add(1)
		t.openLogFile(fullpath, false)
	}
}

// handleLogUpdate reads all available bytes from an already opened *File
// identified by pathname, and sends them to be processed.
func (t *tailer) handleLogUpdate(pathname string) {
Loop:
	for {
		b := make([]byte, 4096)
		n, err := t.files[pathname].Read(b)
		if err != nil {
			if err == io.EOF && n == 0 {
				// end of file for now, return
				break Loop
			}
			glog.Infof("Failed to read updates from %q: %s", pathname, err)
			return
		} else {
			for i, width := 0, 0; i < len(b) && i < n; i += width {
				var rune rune
				rune, width = utf8.DecodeRune(b[i:])
				switch {
				case rune != '\n':
					t.partials[pathname] += string(rune)
				default:
					// send off line for processing
					t.lines <- t.partials[pathname]
					// reset accumulator
					t.partials[pathname] = ""
				}
			}
		}
	}
}

// inode returns the inode number of a file.
func inode(f os.FileInfo) uint64 {
	return f.Sys().(*syscall.Stat_t).Ino
}

// handleLogCreate handles both new and rotated log files.
func (t *tailer) handleLogCreate(pathname string) {
	if !t.isWatching(pathname) {
		glog.V(1).Info("Not watching path %q, ignoring.\n", pathname)
		return
	}

	t.files_lock.Lock()
	fd, ok := t.files[pathname]
	t.files_lock.Unlock()
	if ok {
		s1, err := fd.Stat()
		if err != nil {
			glog.Infof("Stat failed on %q: %s", t.files[pathname].Name(), err)
			return
		}
		s2, err := os.Stat(pathname)
		if err != nil {
			glog.Infof("Stat failed on %q: %s", pathname, err)
			return
		}
		if inode(s1) != inode(s2) {
			log_rotations.Add(pathname, 1)
			// flush the old log, pathname is still an index into t.files with the old inode.
			t.handleLogUpdate(pathname)
			fd.Close()
			err := t.w.Remove(pathname)
			if err != nil {
				glog.Info("Failed removing watches on", pathname)
			}
			// Always seek to start on log rotation.
			t.openLogFile(pathname, true)
		} else {
			glog.Infof("Path %s already being watched, and inode not changed.",
				pathname)
		}
	} else {
		// Freshly opened log file, never seen before, so do not seek to start.
		t.openLogFile(pathname, true)
	}
}

// openLogFile opens a new log file at pathname, and optionally seeks to the
// start or end of the file. Rotated logs should start at the start, but logs
// opened for the first time start at the end.
func (t *tailer) openLogFile(pathname string, seek_to_start bool) {
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

	fd, err := os.Open(pathname)
	if err != nil {
		// Doesn't exist yet. We're watching the directory, so we'll pick it up
		// again on create; return successfully.
		if os.IsNotExist(err) {
			return
		}
		glog.Infof("Failed to open %q for reading: %s", pathname, err)
		log_errors.Add(pathname, 1)
		return
	}
	t.files_lock.Lock()
	t.files[pathname] = fd

	if seek_to_start {
		t.files[pathname].Seek(0, os.SEEK_SET)
	}

	t.files_lock.Unlock()

	err = t.w.Add(pathname)
	if err != nil {
		glog.Infof("Adding a change watch failed on %q: %s", pathname, err)
	}

	// In case the new log has been written to already, attempt to read the first lines.
	t.handleLogUpdate(pathname)
}

// start is the main event loop for the tailer.
// It receives notification of log file changes from the watcher channel, and
// handles them.
func (t *tailer) start() {
	for {
		select {
		case name := <-t.w.Updates():
			t.handleLogUpdate(name)
		case name := <-t.w.Creates():
			t.handleLogCreate(name)
		case <-t.quit:
			goto end
		}
	}
end:
}

func (t *tailer) Stop() {
	t.quit <- true
	t.w.Close()
}
