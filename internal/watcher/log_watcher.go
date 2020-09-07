// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"context"
	"expvar"
	"fmt"
	"os"
	"path"
	"path/filepath"
	"sync"
	"time"

	"github.com/fsnotify/fsnotify"
	"github.com/golang/glog"
	"github.com/pkg/errors"
)

var (
	errorCount = expvar.NewInt("log_watcher_errors_total")
)

type watch struct {
	ps []Processor
	fi os.FileInfo
}

// LogWatcher implements a Watcher for watching real filesystems.
type LogWatcher struct {
	watcher    *fsnotify.Watcher
	pollTicker *time.Ticker

	watchedMu sync.RWMutex // protects `watched'
	watched   map[string]*watch

	stopTicks chan struct{} // Channel to notify ticker to stop.

	ticksDone  chan struct{} // Channel to notify when the ticks handler is done.
	eventsDone chan struct{} // Channel to notify when the events handler is done.

	closeOnce sync.Once
}

// NewLogWatcher returns a new LogWatcher, or returns an error.
func NewLogWatcher(pollInterval time.Duration, enableFsnotify bool) (*LogWatcher, error) {
	var f *fsnotify.Watcher
	if enableFsnotify {
		var err error
		f, err = fsnotify.NewWatcher()
		if err != nil {
			glog.Warning(err)
		}
	}
	if f == nil && pollInterval == 0 {
		glog.Infof("fsnotify disabled and no poll interval specified; defaulting to 250ms poll")
		pollInterval = time.Millisecond * 250
	}
	w := &LogWatcher{
		watcher: f,
		watched: make(map[string]*watch),
	}
	if pollInterval > 0 {
		w.pollTicker = time.NewTicker(pollInterval)
		w.stopTicks = make(chan struct{})
		w.ticksDone = make(chan struct{})
		go w.runTicks()
		glog.V(2).Infof("started ticker with %s interval", pollInterval)
	}
	if f != nil {
		w.eventsDone = make(chan struct{})
		go w.runEvents()
	}
	return w, nil
}

func (w *LogWatcher) sendEvent(e Event) {
	w.watchedMu.RLock()
	watch, ok := w.watched[e.Pathname]
	w.watchedMu.RUnlock()
	if !ok {
		d := filepath.Dir(e.Pathname)
		w.watchedMu.RLock()
		watch, ok = w.watched[d]
		w.watchedMu.RUnlock()
		if !ok {
			glog.V(2).Infof("No watch for path %q", e.Pathname)
			return
		}
	}
	w.sendWatchedEvent(watch, e)
}

// Send an event to a watch; all locks assumed to be held.
func (w *LogWatcher) sendWatchedEvent(watch *watch, e Event) {
	for _, p := range watch.ps {
		p.ProcessFileEvent(context.TODO(), e)
	}
}

func (w *LogWatcher) runTicks() {
	defer close(w.ticksDone)

	if w.pollTicker == nil {
		return
	}

	for {
		select {
		case <-w.pollTicker.C:
			w.watchedMu.RLock()
			for n, watch := range w.watched {
				w.watchedMu.RUnlock()
				w.pollWatchedPath(n, watch)
				w.watchedMu.RLock()
			}
			w.watchedMu.RUnlock()
		case <-w.stopTicks:
			w.pollTicker.Stop()
			return
		}
	}
}

// pollWatchedPathLocked polls an already-watched path for updates.
func (w *LogWatcher) pollWatchedPath(pathname string, watched *watch) {
	glog.V(2).Infof("Stat %q", pathname)
	fi, err := os.Stat(pathname)
	if err != nil {
		glog.V(1).Info(err)
		return
	}

	// fsnotify does not send update events for the directory itself.
	if fi.IsDir() {
		w.pollDirectory(watched, pathname)
	} else if watched.fi == nil || fi.ModTime().Sub(watched.fi.ModTime()) > 0 {
		glog.V(2).Infof("sending update for %s", pathname)
		w.sendWatchedEvent(watched, Event{Update, pathname})
	}

	glog.V(2).Info("Update fi")
	w.watchedMu.Lock()
	if _, ok := w.watched[pathname]; ok {
		w.watched[pathname].fi = fi
	}
	w.watchedMu.Unlock()
}

func (w *LogWatcher) pollDirectory(parentWatch *watch, pathname string) {
	matches, err := filepath.Glob(path.Join(pathname, "*"))
	if err != nil {
		glog.V(1).Info(err)
		return
	}
	// TODO(jaq): how do we avoid duplicate notifies for things that are already in the watch list?
	for _, match := range matches {
		fi, err := os.Stat(match)
		if err != nil {
			glog.V(1).Info(err)
			continue
		}

		w.watchedMu.RLock()
		watched, ok := w.watched[match]
		w.watchedMu.RUnlock()
		switch {
		case !ok:
			glog.V(2).Infof("sending create for %s", match)
			w.sendWatchedEvent(parentWatch, Event{Create, match})
			w.watchedMu.Lock()
			w.watched[match] = &watch{ps: parentWatch.ps, fi: fi}
			w.watchedMu.Unlock()
		case watched.fi != nil && fi.ModTime().Sub(watched.fi.ModTime()) > 0:
			glog.V(2).Infof("sending update for %s", match)
			w.sendWatchedEvent(watched, Event{Update, match})
			w.watchedMu.Lock()
			w.watched[match].fi = fi
			w.watchedMu.Unlock()
		default:
			glog.V(2).Infof("No modtime change for %s, no send", match)
		}
		if fi.IsDir() {
			w.pollDirectory(parentWatch, match)
		}
	}
}

// runEvents assumes that w.watcher is not nil
func (w *LogWatcher) runEvents() {
	defer close(w.eventsDone)

	// Suck out errors and dump them to the error log.
	go func() {
		for err := range w.watcher.Errors {
			errorCount.Add(1)
			glog.Errorf("fsnotify error: %s\n", err)
		}
	}()

	for e := range w.watcher.Events {
		glog.V(2).Infof("watcher event %v", e)
		switch {
		case e.Op&fsnotify.Create == fsnotify.Create:
			w.sendEvent(Event{Create, e.Name})
		case e.Op&fsnotify.Write == fsnotify.Write,
			e.Op&fsnotify.Chmod == fsnotify.Chmod:
			w.sendEvent(Event{Update, e.Name})
		case e.Op&fsnotify.Remove == fsnotify.Remove:
			w.sendEvent(Event{Delete, e.Name})
		case e.Op&fsnotify.Rename == fsnotify.Rename:
			// Rename is only issued on the original file path; the new name receives a Create event
			w.sendEvent(Event{Delete, e.Name})
		default:
			panic(fmt.Sprintf("unknown op type %v", e.Op))
		}
	}
	glog.Infof("Shutting down log watcher.")
}

// Close shuts down the LogWatcher.  It is safe to call this from multiple clients.
func (w *LogWatcher) Close() (err error) {
	w.closeOnce.Do(func() {
		if w.watcher != nil {
			err = w.watcher.Close()
			<-w.eventsDone
		}
		if w.pollTicker != nil {
			close(w.stopTicks)
			<-w.ticksDone
		}
	})
	return nil
}

// Observe adds a path to the list of watched items.
// If this path has a new event, then the processor being registered will be sent the event.
func (w *LogWatcher) Observe(path string, processor Processor) error {
	absPath, err := w.addWatch(path)
	if err != nil {
		return err
	}
	w.watchedMu.Lock()
	defer w.watchedMu.Unlock()
	watched, ok := w.watched[absPath]
	if !ok {
		w.watched[absPath] = &watch{ps: []Processor{processor}}
		glog.Infof("No abspath in watched list, added new one for %s", absPath)
		return nil
	}
	for _, p := range watched.ps {
		if p == processor {
			glog.Infof("Found this processor in watched list")
			return nil
		}
	}
	watched.ps = append(watched.ps, processor)
	glog.Infof("appended this processor")
	return nil
}

func (w *LogWatcher) addWatch(path string) (string, error) {
	absPath, err := filepath.Abs(path)
	if err != nil {
		return "", errors.Wrapf(err, "Failed to lookup absolutepath of %q", path)
	}
	glog.V(2).Infof("Adding a watch on resolved path %q", absPath)
	if w.watcher != nil {
		err = w.watcher.Add(absPath)
		if err != nil {
			if os.IsPermission(err) {
				glog.V(2).Infof("Skipping permission denied error on adding a watch.")
			} else {
				return "", errors.Wrapf(err, "Failed to create a new watch on %q", absPath)
			}
		}
	}
	return absPath, nil
}

// IsWatching indicates if the path is being watched. It includes both
// filenames and directories.
func (w *LogWatcher) IsWatching(path string) bool {
	absPath, err := filepath.Abs(path)
	if err != nil {
		glog.V(2).Infof("Couldn't resolve path %q: %s", absPath, err)
		return false
	}
	glog.V(2).Infof("Resolved path for lookup %q", absPath)
	w.watchedMu.RLock()
	_, ok := w.watched[absPath]
	w.watchedMu.RUnlock()
	return ok
}

func (w *LogWatcher) Unobserve(path string, processor Processor) error {
	w.watchedMu.Lock()
	defer w.watchedMu.Unlock()
	_, ok := w.watched[path]
	if !ok {
		return nil
	}

	for i, p := range w.watched[path].ps {
		if p == processor {
			w.watched[path].ps = append(w.watched[path].ps[0:i], w.watched[path].ps[i+1:]...)
			break
		}
	}
	if len(w.watched[path].ps) == 0 {
		delete(w.watched, path)
	}
	if w.watcher != nil {
		return w.watcher.Remove(path)
	}
	return nil
}
