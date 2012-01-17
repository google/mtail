// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
        "exp/inotify"
        "log"
)

const (
        wLogFileMask = inotify.IN_MODIFY | inotify.IN_DELETE_SELF
)

type watcher struct {
        w       *inotify.Watcher
        change  map[string]chan string
        delete  map[string]chan string
        stop    bool    // Signals the loop to stop.
}

// newWather creates a new watcher.
func NewWatcher() *watcher {
        w, err := inotify.NewWatcher()
        if err != nil {
                log.Fatal("Creating an inotify watcher failed:", err)
        }
        return &watcher{w,
                make(map[string]chan string),
                make(map[string]chan string),
                false}
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

// WatchLogFile adds a watch on a log file
func (w *watcher) WatchLogFile(pathname string, change chan string, delete chan string) {
        w.addWatch(pathname, wLogFileMask, change, delete)
}

// start receives events from the inotify.Watcher's event channel, and sends
// change events to the listening channel.
func (w *watcher) start() {
        for {
                select {
                case ev := <-w.w.Event:
                        switch {
                        case ev.Mask&(inotify.IN_DELETE|inotify.IN_DELETE_SELF) != 0:
                                if c, ok := w.delete[ev.Name]; ok {
                                        c <- ev.Name
                                }
                        case ev.Mask&inotify.IN_IGNORED != 0:
                                // Ignore!
                        default:
                                if c, ok := w.change[ev.Name]; ok {
                                        c <- ev.Name
                                }
                        }
                case err := <-w.w.Error:
                        log.Println("watch:", err)
                }
                if w.stop {
                        break
                }
        }
}
