// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// tail is responsible for tailing a log file and extracting new log lines to
// be passed into the virtual machines.

// emtail gets notified on modifications (i.e. appends) to log files that are
// being watched, in order to read the new lines. Log files can also be
// rotated, so emtail is also notified of creates in the log file directory.

package main

import (
	"exp/inotify"
	"expvar"
	"io"
	"log"
	"os"
	"path"
	"syscall"
	"unicode/utf8"
)

var (
	event_count                 = expvar.NewMap("inotify_event_count")
	log_errors_total            = expvar.NewMap("log_errors_total")
	log_permission_denied_total = expvar.NewMap("log_permission_denied_total")
	log_rotations_total         = expvar.NewMap("log_rotations_total")
)

// Set of masks to use to determine different events
const (
	tLogCreateMask = inotify.IN_CREATE | inotify.IN_ONLYDIR
	tLogUpdateMask = inotify.IN_MODIFY
)

// tailer receives notification of changes from the filesystem watcher and
// extracts new log lines from files.
type tailer struct {
	w *inotify.Watcher

	lines    chan string         // Logfile lines being emitted.
	files    map[string]*os.File // File handles for each pathname.
	partials map[string]string   // Accumulator for the currently read line for each pathname.
}

// NewTailer returns a new tailer.
func NewTailer(lines chan string) *tailer {
	w, err := inotify.NewWatcher()
	if err != nil {
		log.Fatal("Creating an inotify watcher failed:", err)
		return nil
	}
	t := &tailer{
		w:        w,
		lines:    lines,
		files:    make(map[string]*os.File),
		partials: make(map[string]string),
	}
	go t.start()
	return t
}

// Tail adds a file to be tailed.
func (t *tailer) Tail(pathname string) bool {
	return t.openLogFile(pathname, false)
}

// handleLogUpdate reads all available bytes from an already opened *File
// identified by pathname, and sends them to be processed.
func (t *tailer) handleLogUpdate(pathname string) {
Loop:
	for {
		b := make([]byte, 32)
		n, err := t.files[pathname].Read(b)
		if err != nil {
			if err == io.EOF && n == 0 {
				// end of file for now, return
				break Loop
			}
			log.Printf("error reading %q: %q\n", pathname, err)
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

func Inode(f os.FileInfo) uint64 {
	return f.Sys().(*syscall.Stat_t).Ino
}

// handleLogCreate handles both new and rotated log files.
func (t *tailer) handleLogCreate(pathname string) {
	if fd, ok := t.files[pathname]; ok {
		s1, err := fd.Stat()
		if err != nil {
			log.Printf("stat failed on %s: %s\n", t.files[pathname].Name(), err)
			return
		}
		s2, err := os.Stat(pathname)
		if err != nil {
			log.Printf("stat failed on %s: %s\n", pathname, err)
			return
		}
		if Inode(s1) != Inode(s2) {
			log_rotations_total.Add(pathname, 1)
			// flush the old log, pathname is still an index into t.files with the old inode.
			t.handleLogUpdate(pathname)
			fd.Close()
			err := t.w.RemoveWatch(pathname)
			if err != nil {
				log.Println("Failed removing watches on", pathname)
			}
			// Always seek to start on log rotation.
			if !t.openLogFile(pathname, true) {
				log.Println("failed opening", pathname)
			}
		} else {
			log.Printf("Path %s already being watched, and inode not changed.\n",
				pathname)
		}
	} else {
		// Freshly opened log file, never seen before, so do not seek to start.
		if !t.openLogFile(pathname, false) {
			log.Println("failed opening", pathname)
		}
	}
}

// openLogFile opens a new log file at pathname, and optionally seeks to the
// start or end of the file. Rotated logs should start at the start, but logs
// opened for the first time start at the end.
func (t *tailer) openLogFile(pathname string, seek_to_start bool) bool {
	var err error
	t.files[pathname], err = os.Open(pathname)
	if err != nil {
		log.Printf("Failed to open %q for reading: %s\n", pathname, err)
		log_errors_total.Add(pathname, 1)
		if os.IsPermission(err) {
			log_permission_denied_total.Add(pathname, 1)
		}
		return false
	}

	if seek_to_start {
		t.files[pathname].Seek(0, os.SEEK_SET)
	} else {
		t.files[pathname].Seek(0, os.SEEK_END)
	}

	if t.w != nil {
		err := t.w.AddWatch(pathname, tLogUpdateMask)
		if err != nil {
			log.Printf("Adding a change watch failed on %q: %s\n", pathname, err)
		}
		err = t.w.AddWatch(path.Dir(pathname), tLogCreateMask)
		if err != nil {
			log.Printf("Adding a create watch failed on %q: %s\n", path.Dir(pathname), err)
		}
	}

	// In case the log is being written to already, attempt to read the first lines now.
	t.handleLogUpdate(pathname)

	return true
}

// start is the main event loop for the tailer.
// It receives notification of log file changes from the watcher channel, and 
// handles them.
func (t *tailer) start() {
	for {
		select {
		case ev := <-t.w.Event:
			if ev.Name == "" {
				log.Printf("No filename given: %q\n", ev.Name)
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
		case err := <-t.w.Error:
			log.Println("watch error:", err)
		}
	}
}
