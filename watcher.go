package main

import (
	"code.google.com/p/go.exp/inotify"
	"github.com/golang/glog"
)

// Watcher is an interface for watching filesystem events.
type Watcher interface {
	AddWatch(string, uint32) error
	Close() error
	RemoveWatch(string) error
	Channels() (chan string, chan string)
}

// InotifyWatcher implements Watcher using the inotify library.
type InotifyWatcher struct {
	*inotify.Watcher
	ChangedLogFile chan string
	NewLogFile     chan string
}

func NewInotifyWatcher() (w *InotifyWatcher, err error) {
	i, err := inotify.NewWatcher()
	if err != nil {
		return
	}
	w = &InotifyWatcher{i, make(chan string), make(chan string)}
	return
}

func (w *Watcher) Channels() (chan string, chan string) {
	return w.ChangedLogFile, w.NewLogFile
}

func (w *Watcher) Start() {
	for {
		select {
		case ev := <-w.Event:
			if ev == nil {
				glog.Info("event received, but was nil.")
				continue
			}
			event_count.Add(ev.String(), 1)
			switch {
			case ev.Mask&tLogUpdateMask != 0:
				w.ChangedLogFile <- ev.Name

			case ev.Mask&tLogCreateMask != 0:
				w.NewLogFile <- ev.Name

			case ev.Mask&inotify.IN_IGNORED != 0:
				// Ignore!

			default:
				glog.Infof("Unexpected event %q", ev)
			}
		case err := <-w.Errors:
			if err != nil {
				glog.Info("inotify watch error:", err)
			} else {
				glog.Info("inotify watch error, but error was nil")
			}
		case <-t.quit:
			goto end
		}
	}
end:
}

// FakeWatcher implements the Watcher interface with an in-memory watcher.
type FakeWatcher struct {
	watches map[string]uint32
}

func (w *FakeWatcher) AddWatch(pathname string, flags uint32) error {
	w.watches[pathname] = flags
	return nil
}

func (w *FakeWatcher) Close() error {
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
