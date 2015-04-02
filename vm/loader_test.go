// Copyright 2013 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"reflect"
	"testing"
	"time"

	"github.com/google/mtail/watcher"
	"github.com/spf13/afero"
)

var progloadertests = []struct {
	watcher.Event
	pathnames []string
}{
	{
		watcher.Event{Type: watcher.Create,
			Pathname: "foo.mtail"},
		[]string{"foo.mtail"},
	},
	{
		watcher.Event{Type: watcher.Create,
			Pathname: "foo.mtail"},
		[]string{"foo.mtail"},
	},
	{
		watcher.Event{Type: watcher.Create,
			Pathname: "bar.mtail"},
		[]string{"foo.mtail", "bar.mtail"},
	},
	{
		watcher.Event{Type: watcher.Update,
			Pathname: "bar.mtail"},
		[]string{"foo.mtail", "bar.mtail"},
	},
	{
		watcher.Event{Type: watcher.Create,
			Pathname: "no.gz"},
		[]string{"foo.mtail", "bar.mtail"},
	},
	{
		watcher.Event{Type: watcher.Delete,
			Pathname: "foo.mtail"},
		[]string{"bar.mtail"},
	},
}

func TestProgLoader(t *testing.T) {
	fake := watcher.NewFakeWatcher()
	fake.Add(".")
	fs := &afero.MemMapFs{}
	l := NewProgLoader(fake, fs)
	for _, tt := range progloadertests {
		l.Lock()
		switch tt.Event.Type {
		case watcher.Create:
			fake.InjectCreate(tt.Event.Pathname)
		case watcher.Delete:
			fake.InjectDelete(tt.Event.Pathname)
		case watcher.Update:
			fake.InjectUpdate(tt.Event.Pathname)
		}
		pathnames := make(map[string]struct{})
		for _, p := range tt.pathnames {
			pathnames[p] = struct{}{}
		}
		l.Unlock()
		time.Sleep(100 * time.Millisecond)
		l.Lock()
		if !reflect.DeepEqual(pathnames, l.pathnames) {
			t.Errorf("Pathnames don't match for event %s.\n\texpected %q\n\treceived %q", tt.Event, pathnames, l.pathnames)
		}
		l.Unlock()
	}
}
