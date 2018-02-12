// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"expvar"
	"fmt"
	"sync"

	"github.com/fsnotify/fsnotify"
	"github.com/golang/glog"
)

var (
	eventCount = expvar.NewMap("log_watcher_event_count")
	errorCount = expvar.NewInt("log_watcher_error_count")
)

// LogWatcher implements a Watcher for watching real filesystems.
type LogWatcher struct {
	*fsnotify.Watcher

	events   []chan Event
	eventsMu sync.RWMutex

	runDone chan struct{} // Channel to respond to Close
}

// NewLogWatcher returns a new LogWatcher, or returns an error.
func NewLogWatcher() (*LogWatcher, error) {
	f, err := fsnotify.NewWatcher()
	if err == nil {
		w := &LogWatcher{
			Watcher: f,
			events:  make([]chan Event, 0),
			runDone: make(chan struct{}),
		}
		go w.run()
		return w, nil
	}
	return nil, err
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
