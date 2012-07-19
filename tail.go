// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"io"
	"log"
	"os"
	"unicode/utf8"
)

type tailer struct {
	lines    chan string         // Logfile lines being emitted.
	changes  chan string         // Notification of changes arriving.
	files    map[string]*os.File // File handles for each pathname.
	partials map[string]string   // Cache of the currently read line before newline from each pathname.
	watcher  *watcher            // Watcher who we inform about new log files.
}

// NewTailer returns a new tailer.
func NewTailer(w *watcher, lines chan string) *tailer {
	return &tailer{
		lines:    lines,
		changes:  make(chan string),
		files:    make(map[string]*os.File),
		partials: make(map[string]string),
		watcher:  w}
}

// Tail adds a file to be tailed.
func (t *tailer) Tail(pathname string) bool {
	var err error
	t.files[pathname], err = os.Open(pathname)
	if err != nil {
		log.Printf("Failed to open %q for reading: %s\n", pathname, err)
		return false
	}
	t.files[pathname].Seek(0, os.SEEK_END)
	if t.watcher != nil {
		t.watcher.WatchLogFile(pathname, t.changes, nil)
	}
	return true
}

// Handle notification of a log update from the watcher.
func (t *tailer) handleLogChange(pathname string) {
Loop:
	for {
		b := make([]byte, 32)
		n, err := t.files[pathname].Read(b)
		if err != nil {
			if err == io.EOF && n == 0 {
				// end of file for now, return
				break Loop
			}
			log.Fatal("read failed:", err)
		} else {
			for i, width := 0, 0; i < len(b) && i < n; i += width {
				var rune rune
				rune, width = utf8.DecodeRune(b[i:])
				switch {
				case rune != '\n':
					t.partials[pathname] += string(rune)
				default:
					// send off line
					t.lines <- t.partials[pathname]
					t.partials[pathname] = ""
				}
			}
		}
	}
}

func (t *tailer) start() {
	for {
		select {
		case path := <-t.changes:
			go t.handleLogChange(path)
		}
	}
}
