// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"expvar"
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
}

// NewLogWatcher returns a new LogWatcher, or returns an error.
func NewLogWatcher() (*LogWatcher, error) {
	f, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, err
	}
	w := &LogWatcher{Watcher: f, events: make([]chan Event, 0)}
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
	// Suck out errors and dump them to the error log.
	go func() {
		for err := range w.Watcher.Errors {
			errorCount.Add(1)
			glog.Errorf("fsnotify error: %s\n", err)
		}
	}()
	for e := range w.Watcher.Events {
		eventCount.Add(e.Name, 1)
		switch {
		case e.Op&fsnotify.Create == fsnotify.Create:
			w.sendEvent(CreateEvent{e.Name})
		case e.Op&fsnotify.Write == fsnotify.Write:
			w.sendEvent(UpdateEvent{e.Name})
		case e.Op&fsnotify.Remove == fsnotify.Remove:
			w.sendEvent(DeleteEvent{e.Name})
		}
	}
	glog.Infof("Shutting down log watcher.")

	w.eventsMu.Lock()
	for _, c := range w.events {
		close(c)
	}
	w.eventsMu.Unlock()
}
