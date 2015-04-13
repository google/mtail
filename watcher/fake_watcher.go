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
	watches map[string]bool
	events  chan Event
}

// NewFakeWatcher returns a fake Watcher for use in tests.
func NewFakeWatcher() *FakeWatcher {
	return &FakeWatcher{
		watches: make(map[string]bool),
		events:  make(chan Event)}
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
	close(w.events)
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
	defer w.RUnlock()
	if w.watches[dirname] {
		w.events <- CreateEvent{name}
	} else {
		glog.Infof("not watching %s to see %s", dirname, name)
	}
}

// InjectUpdate lets a test inject a fake update event.
func (w *FakeWatcher) InjectUpdate(name string) {
	w.RLock()
	defer w.RUnlock()
	if w.watches[name] {
		w.events <- UpdateEvent{name}
	} else {
		glog.Infof("not watching %s", name)
	}
}

// InjectDelete lets a test inject a fake deletion event.
func (w *FakeWatcher) InjectDelete(name string) {
	w.RLock()
	defer w.RUnlock()
	if w.watches[name] {
		w.events <- DeleteEvent{name}
	} else {
		glog.Infof("not watching %s", name)
	}
}

// Watches returns a list of paths being watched.
func (w *FakeWatcher) Watches() (keys []string) {
	w.RLock()
	defer w.RUnlock()
	keys = make([]string, 0, len(w.watches))
	for k := range w.watches {
		keys = append(keys, k)
	}
	return
}
