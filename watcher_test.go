package main

import (
	"testing"

	"code.google.com/p/go.exp/inotify"
)

func TestFakeWatcher(t *testing.T) {
	w, err := NewFakeWatcher()
	if err != nil {
		t.Errorf("Couldn't make a fake watcher: %s\n", err)
	}

	w.AddWatch("/tmp", tLogCreateMask)
	if !w.IsWatching("/tmp") {
		t.Errorf("Not watching /tmp, w contains: %+#v", w.watches)
	}

	err = w.RemoveWatch("/tmp")
	if err != nil {
		t.Errorf("Couldn't remove watch on /tmp: %s\n", err)
	}

	w.AddWatch("/tmp", tLogCreateMask)
	w.InjectEvent(&inotify.Event{Name: "/tmp"})
	var ev *inotify.Event
	select {
	case ev = <-w.Events():
		if ev.Name != "/tmp" {
			t.Errorf("event doesn't match: %s\n", ev.Name)
		}
	default:
	}

	if ev == nil {
		t.Errorf("no event found in watcher: %+#v\n", w)
	}
	w.AddWatch("/tmp/foo", tLogUpdateMask)
	w.InjectEvent(&inotify.Event{Name: "/tmp/foo"})
	select {
	case ev = <-w.Events():
		if ev.Name != "/tmp/foo" {
			t.Errorf("event doesn't match name: %s\n", ev.Name)
		}
	default:
		t.Errorf("no event found in watcher: %+#v\n", w)
	}
	w.Close()

}
