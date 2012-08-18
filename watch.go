// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Watch the filesystem for updates to log files and programs.

// emtail gets notified on modifications (i.e. appends) to log files that are
// being watched, in order to read the new lines. Log files can also be rotated,
// so emtail is also notified of moves and creates in the log file directory.

// emtail programs may be updated while emtail is running, and they will be
// reloaded without having to restart the emtail process. Programs can be created
// and deleted as well, and some configuration systems do an atomic rename of the
// program when it is installed, so emtail is also aware of file moves.

// The internals of file and directory watching are provided by inotify, the
// watcher merely abstracts the details away.
package main

import (
	"exp/inotify"
	"log"
)

const (
	wLogFileMask = inotify.IN_MODIFY | inotify.IN_DELETE_SELF
	wLogDirMask  = inotify.IN_CREATE | inotify.IN_MOVED_FROM | inotify.IN_MOVED_TO | inotify.IN_ISDIR
	// Writable files get both CLOSE and ATTRIB, nonwritable only get ATTRIB, when touched.
	wProgFileMask = inotify.IN_CLOSE_WRITE | inotify.IN_ATTRIB | inotify.IN_DELETE_SELF
	wProgDirMask  = inotify.IN_CREATE | inotify.IN_MOVED_TO | inotify.IN_ISDIR
)

// watcher keeps filesystem watching state.
type watcher struct {
	w *inotify.Watcher

	// change and delete are maps of channels to notify on the respective action, keyed on full pathname.
	change map[string]chan string
	delete map[string]chan string
}

// newWatcher creates a new watcher.
func NewWatcher() *watcher {
	w, err := inotify.NewWatcher()
	if err != nil {
		log.Fatal("Creating an inotify watcher failed:", err)
	}
	rw := &watcher{w,
		make(map[string]chan string),
		make(map[string]chan string),
	}
	go rw.start()
	return rw
}

// addWatch sets up a watch on a pathname, and sets some channels that will receive events.
func (w *watcher) addWatch(pathname string, flags uint32, change chan string, delete chan string) {
	w.change[pathname] = change
	w.delete[pathname] = delete
	err := w.w.AddWatch(pathname, flags)
	if err != nil {
		log.Printf("Adding a watch failed on %s: %s\n", pathname, err)
	}
}

// WatchLogFile adds a watch on a log file.
func (w *watcher) WatchLogFile(pathname string, change chan string, delete chan string) {
	w.addWatch(pathname, wLogFileMask, change, delete)
}

// WatchLogDir adds a watch on a log directory.
func (w *watcher) WatchLogDir(pathname string, change chan string, delete chan string) {
	w.addWatch(pathname, wLogDirMask, change, delete)
}

// WatchLogDir adds a watch on a program file.
func (w *watcher) WatchProgFile(pathname string, change chan string, delete chan string) {
	w.addWatch(pathname, wProgFileMask, change, delete)
}

// WatchLogDir adds a watch on a program directory.
func (w *watcher) WatchProgDir(pathname string, change chan string, delete chan string) {
	w.addWatch(pathname, wProgDirMask, change, delete)
}

// RemoveAllWatches removes the watches on a file path.
func (w *watcher) RemoveAllWatches(pathname string) {
	err := w.w.RemoveWatch(pathname)
	if err != nil {
		log.Println("watch remove error:", err)
	}
}

// start receives events from the inotify.Watcher's event channel, decodes
// their meaning, and sends change or delete events to the listening channel.
func (w *watcher) start() {
	for {
		select {
		case ev := <-w.w.Event:
			switch {
			case ev.Mask&(inotify.IN_DELETE|inotify.IN_DELETE_SELF) != 0:
				if c, ok := w.delete[ev.Name]; ok {
					c <- ev.Name
				} else {
					log.Printf("watch: No channel defined for event %s on path %s",
						ev, ev.Name)
				}
			case ev.Mask&inotify.IN_IGNORED != 0:
				// Ignore!
			default:
				if c, ok := w.change[ev.Name]; ok {
					c <- ev.Name
				} else {
					log.Printf("watch: No channel defined for event %s on path %s",
						ev, ev.Name)
				}
			}
		case err := <-w.w.Error:
			log.Println("watch error:", err)
		}
	}
}
