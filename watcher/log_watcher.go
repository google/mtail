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
	creates chan string
	updates chan string
	deletes chan string
	quit    chan bool
}

func NewLogWatcher() (w *LogWatcher, err error) {
	f, err := fsnotify.NewWatcher()
	if err != nil {
		return
	}
	w = &LogWatcher{f, make(chan string), make(chan string), make(chan string), make(chan bool)}
	go w.run()
	return
}

func (w *LogWatcher) Creates() chan string { return w.creates }
func (w *LogWatcher) Updates() chan string { return w.updates }
func (w *LogWatcher) Deletes() chan string { return w.deletes }

func (w *LogWatcher) run() {
	for {
		select {
		case e, more := <-w.Events:
			event_count.Add(e.Name, 1)
			glog.Infof("Writing %v", e)
			switch {
			case e.Op&fsnotify.Create == fsnotify.Create:
				w.creates <- e.Name
			case e.Op&fsnotify.Write == fsnotify.Write:
				w.updates <- e.Name
			case e.Op&fsnotify.Remove == fsnotify.Remove:
				w.deletes <- e.Name
			}
			if !more {
				goto end
			}
		case err := <-w.Errors:
			glog.Infof("fsnotify error: %s\n", err)
		case <-w.quit:
			goto end
		}
	}
end:
	close(w.creates)
	close(w.updates)
	close(w.deletes)
}

func (w *LogWatcher) Close() (err error) {
	w.quit <- true
	err = w.Watcher.Close()
	return
}
