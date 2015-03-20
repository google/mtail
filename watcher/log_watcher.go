package watcher

import (
	"github.com/golang/glog"
	"gopkg.in/fsnotify.v1"
)

type LogWatcher struct {
	*fsnotify.Watcher
	creates chan string
	updates chan string
	quit    chan bool
}

func NewLogWatcher() (w *LogWatcher, err error) {
	f, err := fsnotify.NewWatcher()
	if err != nil {
		return
	}
	w = &LogWatcher{f, make(chan string), make(chan string), make(chan bool)}
	go w.run()
	return
}

func (w *LogWatcher) Creates() chan string { return w.creates }
func (w *LogWatcher) Updates() chan string { return w.updates }

func (w *LogWatcher) run() {
	for {
		select {
		case e, more := <-w.Events:
			if !more {
				goto end
			}
			switch {
			case e.Op&fsnotify.Create == fsnotify.Create:
				w.creates <- e.Name
			case e.Op&fsnotify.Write == fsnotify.Write:
				w.updates <- e.Name
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
	close(w.quit)
}

func (w *LogWatcher) Close() error {
	w.quit <- true
	close(w.creates)
	close(w.updates)
	return w.Watcher.Close()
}
