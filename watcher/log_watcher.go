// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"expvar"

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
	events []chan Event
}

// NewLogWatcher returns a new LogWatcher, or returns an error.
func NewLogWatcher() (*LogWatcher, error) {
	f, err := fsnotify.NewWatcher()
	if err == nil {
		w := &LogWatcher{f, make([]chan Event, 0)}
		go w.run()
		return w, nil
	}
	return nil, err
}

// Events returns a new readable channel of events from this watcher.
func (w *LogWatcher) Events() <-chan Event {
	r := make(chan Event, 1)
	w.events = append(w.events, r)
	return r
}

func (w *LogWatcher) sendEvent(e Event) {
	for _, c := range w.events {
		c <- e
	}
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
	for _, c := range w.events {
		close(c)
	}
}
