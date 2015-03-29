// Copyright 2013 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"reflect"
	"testing"
	"time"

	"code.google.com/p/go.exp/inotify"
	"github.com/google/mtail/watcher"
)

type fakewatcher struct {
	inotify.Watcher
}

func (f *fakewatcher) AddWatch(string, uint32) error { return nil }
func (f *fakewatcher) Close() error                  { return nil }
func (f *fakewatcher) RemoveWatch(string) error      { return nil }
func (f *fakewatcher) Watch(string) error            { return nil }
func (f *fakewatcher) Errors() chan error            { return f.Error }
func (f *fakewatcher) Events() chan *inotify.Event   { return f.Event }

var progloadertests = []struct {
	*inotify.Event
	pathnames []string
}{
	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "foo.mtail"},
		[]string{"foo.mtail"},
	},
	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "foo.mtail"},
		[]string{"foo.mtail"},
	},
	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "bar.mtail"},
		[]string{"foo.mtail", "bar.mtail"},
	},
	{
		&inotify.Event{Mask: inotify.IN_MODIFY,
			Cookie: 0,
			Name:   "bar.mtail"},
		[]string{"foo.mtail", "bar.mtail"},
	},
	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "no.gz"},
		[]string{"foo.mtail", "bar.mtail"},
	},
	{
		&inotify.Event{Mask: inotify.IN_DELETE,
			Cookie: 0,
			Name:   "foo.mtail"},
		[]string{"bar.mtail"},
	},
}

func TestProgLoader(t *testing.T) {
	fake := watcher.NewFakeWatcher()
	fake.Add(".")
	l := NewProgLoader(fake)
	for _, tt := range progloadertests {
		l.Lock()
		switch tt.Event.Mask {
		case inotify.IN_CREATE:
			fake.InjectCreate(tt.Name)
		case inotify.IN_DELETE:
			fake.InjectDelete(tt.Name)
		case inotify.IN_MODIFY:
			fake.InjectUpdate(tt.Name)
		}
		pathnames := make(map[string]struct{})
		for _, p := range tt.pathnames {
			pathnames[p] = struct{}{}
		}
		l.Unlock()
		time.Sleep(100 * time.Millisecond)
		l.Lock()
		if !reflect.DeepEqual(pathnames, l.pathnames) {
			t.Errorf("Pathnames don't match for event %s.\n\texpected %q\n\treceived %q", tt.Event.String(), pathnames, l.pathnames)
		}
		l.Unlock()
	}
}
