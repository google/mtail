// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"context"
	"path"
	"sync"

	"github.com/golang/glog"
)

// FakeWatcher implements an in-memory Watcher.
type FakeWatcher struct {
	watchesMu sync.RWMutex
	watches   map[string]map[Processor]struct{}
	isClosed  bool
}

// NewFakeWatcher returns a fake Watcher for use in tests.
func NewFakeWatcher() *FakeWatcher {
	return &FakeWatcher{
		watches: make(map[string]map[Processor]struct{})}
}

func (w *FakeWatcher) Observe(name string, p Processor) error {
	w.watchesMu.Lock()
	defer w.watchesMu.Unlock()
	_, ok := w.watches[name]
	if !ok {
		w.watches[name] = make(map[Processor]struct{})
	}
	w.watches[name][p] = struct{}{}
	return nil
}

// Close closes down the FakeWatcher
func (w *FakeWatcher) Close() error {
	w.isClosed = true
	return nil
}

// Unobserve removes an observer from the FakeWatcher.  If it's the last
// observer for a name, the name is no longer watched.
func (w *FakeWatcher) Unobserve(name string, p Processor) error {
	w.watchesMu.Lock()
	defer w.watchesMu.Unlock()

	_, ok := w.watches[name]
	if !ok {
		return nil
	}
	delete(w.watches[name], p)
	if len(w.watches[name]) == 0 {
		delete(w.watches, name)
	}
	return nil
}

func (w *FakeWatcher) SendEvent(e Event) {
	w.watchesMu.RLock()
	name := e.Pathname
	if e.Op == Create {
		name = path.Dir(name)
	}
	watches, ok := w.watches[name]
	w.watchesMu.RUnlock()
	if !ok {
		glog.Infof("Didn't find %s in watched list", name)
		return
	}
	for p := range watches {
		p.ProcessFileEvent(context.Background(), e)
	}
}

// InjectCreate lets a test inject a fake creation event.
func (w *FakeWatcher) InjectCreate(name string) {
	dirname := path.Dir(name)
	w.watchesMu.RLock()
	_, dirWatched := w.watches[dirname]
	w.watchesMu.RUnlock()
	if !dirWatched {
		glog.Warningf("not watching %s to see %s", dirname, name)
		return
	}
	w.SendEvent(Event{Create, name})
}

// InjectUpdate lets a test inject a fake update event.
func (w *FakeWatcher) InjectUpdate(name string) {
	w.watchesMu.RLock()
	_, watched := w.watches[name]
	w.watchesMu.RUnlock()
	if !watched {
		glog.Warningf("can't update: not watching %s", name)
		return
	}
	w.SendEvent(Event{Update, name})
}

// InjectDelete lets a test inject a fake deletion event.
func (w *FakeWatcher) InjectDelete(name string) {
	w.watchesMu.RLock()
	_, watched := w.watches[name]
	w.watchesMu.RUnlock()
	if !watched {
		glog.Warningf("can't delete: not watching %s", name)
		return
	}
	w.SendEvent(Event{Delete, name})
}

// Poll does nothing in the fake watcher; events are injected.
func (w *FakeWatcher) Poll() {
}
