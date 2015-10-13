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
	sync.RWMutex
	watches  map[string]bool
	events   chan Event
	isClosed bool
}

// NewFakeWatcher returns a fake Watcher for use in tests.
func NewFakeWatcher() *FakeWatcher {
	return &FakeWatcher{
		watches: make(map[string]bool),
		events:  make(chan Event, 1)}
}

// Add adds a watch to the FakeWatcher
func (w *FakeWatcher) Add(name string) error {
	w.Lock()
	w.watches[name] = true
	w.Unlock()
	return nil
}

// Close closes down the FakeWatcher
func (w *FakeWatcher) Close() error {
	if !w.isClosed {
		close(w.events)
		w.isClosed = true
	}
	return nil
}

// Remove removes a watch from the FakeWatcher
func (w *FakeWatcher) Remove(name string) error {
	w.Lock()
	delete(w.watches, name)
	w.Unlock()
	return nil
}

// Events returns the channel of messages.
func (w *FakeWatcher) Events() <-chan Event { return w.events }

// InjectCreate lets a test inject a fake creation event.
func (w *FakeWatcher) InjectCreate(name string) {
	dirname := path.Dir(name)
	w.RLock()
	dir_watched := w.watches[dirname]
	w.RUnlock()
	if dir_watched {
		w.events <- CreateEvent{name}
		w.Add(name)
	} else {
		glog.Warningf("not watching %s to see %s", dirname, name)
	}
}

// InjectUpdate lets a test inject a fake update event.
func (w *FakeWatcher) InjectUpdate(name string) {
	w.RLock()
	defer w.RUnlock()
	if w.watches[name] {
		w.events <- UpdateEvent{name}
	} else {
		glog.Warningf("can't update: not watching %s", name)
	}
}

// InjectDelete lets a test inject a fake deletion event.
func (w *FakeWatcher) InjectDelete(name string) {
	w.RLock()
	watched := w.watches[name]
	w.RUnlock()
	if watched {
		w.events <- DeleteEvent{name}
		w.Remove(name)
	} else {
		glog.Warningf("can't delete: not watching %s", name)
	}
}
