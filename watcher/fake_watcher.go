// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"path"
	"sync"

	"github.com/golang/glog"
)

// FakeWatcher implements an in-memory Watcher.
type FakeWatcher struct {
	watchesMu sync.RWMutex
	watches   map[string]bool
	eventsMu  sync.RWMutex
	events    []chan Event
	isClosed  bool
}

// NewFakeWatcher returns a fake Watcher for use in tests.
func NewFakeWatcher() *FakeWatcher {
	return &FakeWatcher{
		watches: make(map[string]bool)}
}

// Add adds a watch to the FakeWatcher
func (w *FakeWatcher) Add(name string) error {
	w.watchesMu.Lock()
	w.watches[name] = true
	w.watchesMu.Unlock()
	return nil
}

// Close closes down the FakeWatcher
func (w *FakeWatcher) Close() error {
	w.eventsMu.Lock()
	defer w.eventsMu.Unlock()
	if !w.isClosed {
		for _, c := range w.events {
			close(c)
		}
		w.isClosed = true
	}
	return nil
}

// Remove removes a watch from the FakeWatcher
func (w *FakeWatcher) Remove(name string) error {
	w.watchesMu.Lock()
	delete(w.watches, name)
	w.watchesMu.Unlock()
	return nil
}

// Events returns a new channel of messages.
func (w *FakeWatcher) Events() <-chan Event {
	w.eventsMu.Lock()
	defer w.eventsMu.Unlock()
	if w.isClosed {
		panic("closed")
	}
	r := make(chan Event, 1)
	w.events = append(w.events, r)
	return r
}

func (w *FakeWatcher) sendEvent(e Event) {
	w.eventsMu.RLock()
	defer w.eventsMu.RUnlock()
	for _, c := range w.events {
		c <- e
	}
}

// InjectCreate lets a test inject a fake creation event.
func (w *FakeWatcher) InjectCreate(name string) {
	dirname := path.Dir(name)
	w.watchesMu.RLock()
	dir_watched := w.watches[dirname]
	w.watchesMu.RUnlock()
	if dir_watched {
		w.sendEvent(CreateEvent{name})
		w.Add(name)
	} else {
		glog.Warningf("not watching %s to see %s", dirname, name)
	}
}

// InjectUpdate lets a test inject a fake update event.
func (w *FakeWatcher) InjectUpdate(name string) {
	w.watchesMu.RLock()
	watched := w.watches[name]
	w.watchesMu.RUnlock()
	if watched {
		w.sendEvent(UpdateEvent{name})
	} else {
		glog.Warningf("can't update: not watching %s", name)
	}
}

// InjectDelete lets a test inject a fake deletion event.
func (w *FakeWatcher) InjectDelete(name string) {
	w.watchesMu.RLock()
	watched := w.watches[name]
	w.watchesMu.RUnlock()
	if watched {
		w.sendEvent(DeleteEvent{name})
		w.Remove(name)
	} else {
		glog.Warningf("can't delete: not watching %s", name)
	}
}
