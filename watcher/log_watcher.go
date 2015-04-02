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
	quit   chan struct{}
}

func NewLogWatcher() (w *LogWatcher, err error) {
	f, err := fsnotify.NewWatcher()
	if err != nil {
		return
	}
	w = &LogWatcher{f, make(chan Event), make(chan struct{})}
	go w.run()
	return
}

func (w *LogWatcher) Events() <-chan Event { return w.events }

func (w *LogWatcher) run() {
	for {
		select {
		case e, more := <-w.Watcher.Events:
			event_count.Add(e.Name, 1)
			glog.Infof("Writing %v", e)
			event := Event{Pathname: e.Name}
			switch {
			case e.Op&fsnotify.Create == fsnotify.Create:
				event.Type = Create
			case e.Op&fsnotify.Write == fsnotify.Write:
				event.Type = Update
			case e.Op&fsnotify.Remove == fsnotify.Remove:
				event.Type = Delete
			default:
				glog.Infof("Unexpected event type detected: %q", e)
			}
			w.events <- event
			if !more {
				goto end
			}
		case err := <-w.Errors:
			glog.Errorf("fsnotify error: %s\n", err)
		case <-w.quit:
			goto end
		}
	}
end:
	close(w.events)
}

func (w *LogWatcher) Close() error {
	close(w.quit)
	return w.Watcher.Close()
}
