// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"expvar"
	"fmt"
	"os"
	"path/filepath"
	"sync"

	"github.com/fsnotify/fsnotify"
	"github.com/golang/glog"
	"github.com/pkg/errors"
)

var (
	eventCount = expvar.NewMap("log_watcher_event_count")
	errorCount = expvar.NewInt("log_watcher_error_count")
)

// LogWatcher implements a Watcher for watching real filesystems.
type LogWatcher struct {
	*fsnotify.Watcher

	eventsMu sync.RWMutex
	events   []chan Event

	watchedMu sync.RWMutex        // protects `watched'
	watched   map[string]struct{} // Names of paths being watched

	runDone chan struct{} // Channel to respond to Close
}

// NewLogWatcher returns a new LogWatcher, or returns an error.
func NewLogWatcher() (*LogWatcher, error) {
	f, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, err
	}
	w := &LogWatcher{
		Watcher: f,
		events:  make([]chan Event, 0),
		watched: make(map[string]struct{}),
		runDone: make(chan struct{}),
	}
	go w.run()
	return w, nil
}

// Events returns a new readable channel of events from this watcher.
func (w *LogWatcher) Events() <-chan Event {
	w.eventsMu.Lock()
	defer w.eventsMu.Unlock()
	r := make(chan Event, 1)
	w.events = append(w.events, r)
	return r
}

func (w *LogWatcher) sendEvent(e Event) {
	w.eventsMu.RLock()
	for _, c := range w.events {
		c <- e
	}
	w.eventsMu.RUnlock()
}

func (w *LogWatcher) run() {
	defer close(w.runDone)
	// Suck out errors and dump them to the error log.
	go func() {
		for err := range w.Watcher.Errors {
			errorCount.Add(1)
			glog.Errorf("fsnotify error: %s\n", err)
		}
	}()
	for e := range w.Watcher.Events {
		glog.V(2).Infof("watcher event %v", e)
		eventCount.Add(e.Name, 1)
		switch {
		case e.Op&fsnotify.Create == fsnotify.Create:
			w.sendEvent(CreateEvent{e.Name})
		case e.Op&fsnotify.Write == fsnotify.Write,
			e.Op&fsnotify.Chmod == fsnotify.Chmod:
			w.sendEvent(UpdateEvent{e.Name})
		case e.Op&fsnotify.Remove == fsnotify.Remove:
			w.sendEvent(DeleteEvent{e.Name})
		case e.Op&fsnotify.Rename == fsnotify.Rename:
			// Rename is only issued on the original file path; the new name receives a Create event
			w.sendEvent(DeleteEvent{e.Name})
		default:
			panic(fmt.Sprintf("unknown op type %v", e.Op))
		}
	}
	glog.Infof("Shutting down log watcher.")

	w.eventsMu.Lock()
	for _, c := range w.events {
		close(c)
	}
	w.eventsMu.Unlock()
}

// Close shuts down the LogWatcher.  It is safe to call this from multiple clients because
func (w *LogWatcher) Close() (err error) {
	err = w.Watcher.Close()
	<-w.runDone
	return
}

// Add adds a path to the list of watched items.
func (w *LogWatcher) Add(path string) error {
	if w.IsWatching(path) {
		return nil
	}
	absPath, err := filepath.Abs(path)
	if err != nil {
		return errors.Wrapf(err, "Failed to lookup absolutepath of %q", path)
	}
	glog.V(2).Infof("Adding a watch on resolved path %q", absPath)
	err = w.Watcher.Add(absPath)
	if err != nil {
		if os.IsPermission(err) {
			glog.V(2).Infof("Skipping permission denied error on adding a watch.")
		} else {
			return errors.Wrapf(err, "Failed to create a new watch on %q", absPath)
		}
	}
	w.watchedMu.Lock()
	defer w.watchedMu.Unlock()
	w.watched[absPath] = struct{}{}
	return nil
}

// IsWatching indicates if the path is being watched. It includes both
// filenames and directories.
func (w *LogWatcher) IsWatching(path string) bool {
	absPath, err := filepath.Abs(path)
	if err != nil {
		glog.V(2).Infof("Couldn't resolve path %q: %s", absPath, err)
		return false
	}
	glog.V(2).Infof("Resolved path for lookup %q", absPath)
	w.watchedMu.RLock()
	defer w.watchedMu.RUnlock()
	_, ok := w.watched[absPath]
	return ok
}
