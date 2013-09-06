package main

import (
	"code.google.com/p/go.exp/inotify"
)

type Watcher interface {
	Events() chan *inotify.Event
	Errors() chan error

	AddWatch(string, uint32) error
	Close() error
	RemoveWatch(string) error
}

type InotifyWatcher struct {
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
