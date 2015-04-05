package watcher

import (
	"expvar"

	"github.com/golang/glog"
	"gopkg.in/fsnotify.v1"
)

var (
	event_count = expvar.NewMap("log_watcher_event_count")
)

type LogWatcher struct {
	*fsnotify.Watcher
	events chan Event
}

func NewLogWatcher() (w *LogWatcher, err error) {
	f, err := fsnotify.NewWatcher()
	if err != nil {
		return
	}
	w = &LogWatcher{f, make(chan Event)}
	go w.run()
	return
}

func (w *LogWatcher) Events() <-chan Event { return w.events }

func (w *LogWatcher) run() {
	// Suck out errors and dump them to the error log.
	go func() {
		for err := range w.Watcher.Errors {
			glog.Errorf("fsnotify error: %s\n", err)
		}
	}()
	for e := range w.Watcher.Events {
		event_count.Add(e.Name, 1)
		switch {
		case e.Op&fsnotify.Create == fsnotify.Create:
			w.events <- CreateEvent{e.Name}
		case e.Op&fsnotify.Write == fsnotify.Write:
			w.events <- UpdateEvent{e.Name}
		case e.Op&fsnotify.Remove == fsnotify.Remove:
			w.events <- DeleteEvent{e.Name}
		default:
			glog.Infof("Unexpected event type detected: %v", e)
		}
	}
	glog.Infof("Shutting down log watcher.")
	close(w.events)
}
