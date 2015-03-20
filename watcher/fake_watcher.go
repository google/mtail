package watcher

import "github.com/golang/glog"

// FakeWatcher implements an in-memory Watcher.
type FakeWatcher struct {
	watches map[string]bool
	creates chan string
	updates chan string
}

func NewFakeWatcher() *FakeWatcher {
	return &FakeWatcher{
		make(map[string]bool),
		make(chan string, 1),
		make(chan string, 1)}
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
	return nil
}

// Remove removes a watch from the FakeWatcher
func (w *FakeWatcher) Remove(name string) error {
	delete(w.watches, name)
	return nil
}

// Creates and Updates return the channel of log creation and update messages.
func (w *FakeWatcher) Creates() chan string { return w.creates }
func (w *FakeWatcher) Updates() chan string { return w.updates }

// InjectCreate lets a test inject a fake log creation event.
func (w *FakeWatcher) InjectCreate(name string) {
	if w.watches[name] {
		w.creates <- name
	} else {
		glog.Infof("not watching %s", name)
	}
}

// InjectUpdate lets a test inject a fake log update event.
func (w *FakeWatcher) InjectUpdate(name string) {
	if w.watches[name] {
		w.updates <- name
	} else {
		glog.Infof("not watching %s", name)
	}
}
