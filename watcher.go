package main

import "code.google.com/p/go.exp/inotify"

// Watcher is an interface for watching filesystem events.
type Watcher interface {
	Events() chan *inotify.Event
	Errors() chan error

	AddWatch(string, uint32) error
	Close() error
	RemoveWatch(string) error
}

// InotifyWatcher implements Watcher using the inotify library.
type FsNotifyWatcher struct {
	*inotify.Watcher
}

func (w *InotifyWatcher) Events() chan *inotify.Event { return w.Event }
func (w *InotifyWatcher) Errors() chan error          { return w.Error }

func NewInotifyWatcher() (w *InotifyWatcher, err error) {
	i, err := inotify.NewWatcher()
	if err != nil {
		return
	}
	w = &InotifyWatcher{i}
	return
}

// FakeWatcher implements the Watcher interface with an in-memory watcher.
type FakeWatcher struct {
	watches map[string]uint32
	events  chan *inotify.Event
	errors  chan error
}

func (w *FakeWatcher) Events() chan *inotify.Event {
	return w.events
}

func (w *FakeWatcher) Errors() chan error {
	return w.errors
}

func (w *FakeWatcher) AddWatch(pathname string, flags uint32) error {
	w.watches[pathname] = flags
	return nil
}

func (w *FakeWatcher) Close() error {
	close(w.events)
	close(w.errors)
	return nil
}

func (w *FakeWatcher) RemoveWatch(pathname string) error {
	delete(w.watches, pathname)
	return nil
}

func NewFakeWatcher() (w *FakeWatcher, err error) {
	return &FakeWatcher{
		watches: make(map[string]uint32),
		events:  make(chan *inotify.Event, 1),
		errors:  make(chan error, 1)}, nil
}

func (w *FakeWatcher) InjectEvent(e *inotify.Event) {
	w.events <- e
}

func (w *FakeWatcher) InjectError(e error) {
	w.errors <- e
}

func (w *FakeWatcher) IsWatching(pathname string) bool {
	_, ok := w.watches[pathname]
	return ok
}
