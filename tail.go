// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// tail is responsible for tailing a log file and extracting new log lines to
// be passed into the virtual machines.

// emtail gets notified on modifications (i.e. appends) to log files that are
// being watched, in order to read the new lines. Log files can also be
// rotated, so emtail is also notified of creates in the log file directory.

package main

import (
	"code.google.com/p/go.exp/inotify"
	"expvar"
	"io"
	"log"
	"os"
	"path"
	"path/filepath"
	"syscall"
	"unicode/utf8"
)

var (
	event_count   = expvar.NewMap("inotify_event_count")
	log_count     = expvar.NewInt("log_count")
	log_errors    = expvar.NewMap("log_errors_total")
	log_rotations = expvar.NewMap("log_rotations_total")
)

// Set of masks to use to determine different events
const (
	tLogCreateMask = inotify.IN_CREATE | inotify.IN_ONLYDIR
	tLogUpdateMask = inotify.IN_MODIFY
)

// tailer receives notification of changes from inotify and extracts new log
// lines from files. It also handles new log file creation events and log
// rotations.
type tailer struct {
	w Watcher

	quit chan bool

	watched  map[string]struct{} // Names of logs being watched.
	lines    chan string         // Logfile lines being emitted.
	files    map[string]*os.File // File handles for each pathname.
	partials map[string]string   // Accumulator for the currently read line for each pathname.
}

// NewTailer returns a new tailer.
func NewTailer(lines chan string, w Watcher) *tailer {
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

// Tail registers a file to be tailed.
func (t *tailer) Tail(pathname string) {
	fullpath, err := filepath.Abs(pathname)
	if err != nil {
		log.Printf("Failed to find absolute path for %q: %s\n", pathname, err)
		return
	}
	if _, ok := t.watched[fullpath]; !ok {
		// Mark this file as watchable.
		t.watched[fullpath] = struct{}{}
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
			log.Printf("Failed to read updates from %q: %s\n", pathname, err)
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
	if _, ok := t.watched[pathname]; !ok {
		log.Printf("Not watching path %q, ignoring.\n", pathname)
		return
	}

	if fd, ok := t.files[pathname]; ok {
		s1, err := fd.Stat()
		if err != nil {
			log.Printf("Stat failed on %q: %s\n", t.files[pathname].Name(), err)
			return
		}
		s2, err := os.Stat(pathname)
		if err != nil {
			log.Printf("Stat failed on %q: %s\n", pathname, err)
			return
		}
		if inode(s1) != inode(s2) {
			log_rotations.Add(pathname, 1)
			// flush the old log, pathname is still an index into t.files with the old inode.
			t.handleLogUpdate(pathname)
			fd.Close()
			err := t.w.RemoveWatch(pathname)
			if err != nil {
				log.Println("Failed removing watches on", pathname)
			}
			// Always seek to start on log rotation.
			t.openLogFile(pathname, true)
		} else {
			log.Printf("Path %s already being watched, and inode not changed.\n",
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
	if _, ok := t.watched[pathname]; !ok {
		log.Printf("Not watching %q, ignoring.\n", pathname)
		return
	}

	d := path.Dir(pathname)
	if _, ok := t.watched[d]; !ok {
		err := t.w.AddWatch(d, tLogCreateMask)
		if err != nil {
			log.Printf("Adding a create watch failed on %q: %s\n", d, err)
		}
		t.watched[d] = struct{}{}
	}

	var err error
	t.files[pathname], err = os.Open(pathname)
	if err != nil {
		// Doesn't exist yet. We're watching the directory, so clear this
		// invalid file pointer and return successfully.
		if os.IsNotExist(err) {
			delete(t.files, pathname)
			return
		}
		log.Printf("Failed to open %q for reading: %s\n", pathname, err)
		log_errors.Add(pathname, 1)
		return
	}

	if seek_to_start {
		t.files[pathname].Seek(0, os.SEEK_SET)
	}

	err = t.w.AddWatch(pathname, tLogUpdateMask)
	if err != nil {
		log.Printf("Adding a change watch failed on %q: %s\n", pathname, err)
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
		case ev := <-t.w.Events():
			if ev == nil {
				log.Println("event received, but was nil.")
				continue
			}
			event_count.Add(ev.String(), 1)
			switch {
			case ev.Mask&tLogUpdateMask != 0:
				t.handleLogUpdate(ev.Name)

			case ev.Mask&tLogCreateMask != 0:
				t.handleLogCreate(ev.Name)

			case ev.Mask&inotify.IN_IGNORED != 0:
				// Ignore!

			default:
				log.Printf("Unexpected event %q\n", ev)
			}
		case err := <-t.w.Errors():
			if err != nil {
				log.Println("inotify watch error:", err)
			} else {
				log.Println("inotify watch error, but error was nil")
			}
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
