// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"expvar"

	"github.com/golang/glog"
	"gopkg.in/fsnotify.v1"
)

var (
	eventCount = expvar.NewMap("log_watcher_event_count")
)

// LogWatcher implements a Watcher for watching real filesystems.
type LogWatcher struct {
	*fsnotify.Watcher
	events chan Event
}

// NewLogWatcher returns a new LogWatcher, or returns an error.
func NewLogWatcher() (*LogWatcher, error) {
	f, err := fsnotify.NewWatcher()
	if err == nil {
		w := &LogWatcher{f, make(chan Event)}
		go w.run()
		return w, nil
	}
	return nil, err
}

// Events returns a readable channel of events from this watcher.
func (w *LogWatcher) Events() <-chan Event { return w.events }

func (w *LogWatcher) run() {
	// Suck out errors and dump them to the error log.
	go func() {
		for err := range w.Watcher.Errors {
			glog.Errorf("fsnotify error: %s\n", err)
		}
	}()
	for e := range w.Watcher.Events {
		eventCount.Add(e.Name, 1)
		switch {
		case e.Op&fsnotify.Create == fsnotify.Create:
			w.events <- CreateEvent{e.Name}
		case e.Op&fsnotify.Write == fsnotify.Write:
			w.events <- UpdateEvent{e.Name}
		case e.Op&fsnotify.Remove == fsnotify.Remove:
			w.events <- DeleteEvent{e.Name}
		}
	}
	glog.Infof("Shutting down log watcher.")
	close(w.events)
}
