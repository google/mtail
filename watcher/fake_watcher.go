package watcher

import (
	"path"

	"github.com/golang/glog"
)

// FakeWatcher implements an in-memory Watcher.
type FakeWatcher struct {
	watches map[string]bool
	creates chan string
	updates chan string
	deletes chan string
}

func NewFakeWatcher() *FakeWatcher {
	return &FakeWatcher{
		make(map[string]bool),
		make(chan string),
		make(chan string),
		make(chan string)}
}

// Add adds a watch to the FakeWatcher
func (w *FakeWatcher) Add(name string) error {
	w.watches[name] = true
	return nil
}

// Close closes down the FakeWatcher
func (w *FakeWatcher) Close() error {
	close(w.creates)
	close(w.updates)
	close(w.deletes)
	return nil
}

// Remove removes a watch from the FakeWatcher
func (w *FakeWatcher) Remove(name string) error {
	delete(w.watches, name)
	return nil
}

// Creates, Updates, and Deletes return the channel of messages for their respective event.
func (w *FakeWatcher) Creates() chan string { return w.creates }
func (w *FakeWatcher) Updates() chan string { return w.updates }
func (w *FakeWatcher) Deletes() chan string { return w.deletes }

// InjectCreate lets a test inject a fake creation event.
func (w *FakeWatcher) InjectCreate(name string) {
	dirname := path.Dir(name)
	if w.watches[dirname] {
		w.creates <- name
	} else {
		glog.Infof("not watching %s to see %s", dirname, name)
	}
}

// InjectUpdate lets a test inject a fake update event.
func (w *FakeWatcher) InjectUpdate(name string) {
	if w.watches[name] {
		w.updates <- name
	} else {
		glog.Infof("not watching %s", name)
	}
}

// InjectDelete lets a test inject a fake deletion event.
func (w *FakeWatcher) InjectDelete(name string) {
	if w.watches[name] {
		w.deletes <- name
	} else {
		glog.Infof("not watching %s", name)
	}
}
