// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/watcher"
	"github.com/jaqx0r/afero"
	"github.com/kylelemons/godebug/pretty"
)

func TestNewLoaderWithFs(t *testing.T) {
	w := watcher.NewFakeWatcher()
	store := &metrics.Store{}
	inLines := make(chan string)
	fs := &afero.MemMapFs{}
	l := newLoaderWithFs(w, store, inLines, fs)
	done := make(chan struct{})
	outLines := make(chan string)
	handle := &vmHandle{outLines, done}
	l.handleMu.Lock()
	l.handles["test"] = handle
	l.handleMu.Unlock()
	go func() {
		for _ = range outLines {
		}
		close(done)
	}()
	close(inLines)
	<-outLines
}

func TestCompileAndRun(t *testing.T) {
	var testProgram = `/$/ {}`
	store := &metrics.Store{}
	lines := make(chan string)
	w := watcher.NewFakeWatcher()
	fs := &afero.MemMapFs{}
	l := newLoaderWithFs(w, store, lines, fs)
	if err := l.CompileAndRun("Test", strings.NewReader(testProgram)); err != nil {
		t.Errorf("CompileAndRun returned error: %s", err)
	}
	l.handleMu.Lock()
	if len(l.handles) < 1 {
		t.Errorf("no vm handles: %v", l.handles)
	}
	l.handleMu.Unlock()
	l.handleMu.Lock()
	c := l.handles["Test"].done
	if c == nil {
		t.Errorf("No done channel in handles: %v", l.handles)
	}
	l.handleMu.Unlock()
	close(lines)
	<-c
	{
		l.handleMu.Lock()
		defer l.handleMu.Unlock()
		if len(l.handles) != 0 {
			t.Errorf("some vm handles: %v", l.handles)
		}
	}
}

var testProcessEvents = []struct {
	name             string
	events           []watcher.Event
	expectedPrograms []string
}{
	{"load",
		[]watcher.Event{
			watcher.CreateEvent{"foo.mtail"},
			watcher.UpdateEvent{"foo.mtail"}},
		[]string{"foo.mtail"}},
	{"unload",
		[]watcher.Event{
			watcher.CreateEvent{"foo.mtail"},
			watcher.UpdateEvent{"foo.mtail"},
			watcher.DeleteEvent{"foo.mtail"}},
		[]string{}},
	{"reload",
		[]watcher.Event{
			watcher.CreateEvent{"foo.mtail"},
			watcher.UpdateEvent{"foo.mtail"},
			watcher.UpdateEvent{"foo.mtail"}},
		[]string{"foo.mtail"}},
	{"bad extension",
		[]watcher.Event{
			watcher.CreateEvent{"foo.mtail.dpkg-dist"},
			watcher.UpdateEvent{"foo.mtail.dpkg-dist"}},
		[]string{}},
	{"not exist",
		[]watcher.Event{
			watcher.CreateEvent{"notexist.mtail"},
			watcher.UpdateEvent{"notexist.mtail"}},
		[]string{}},
}

var testProgram = `/$/ {}`

func TestProcessEvents(t *testing.T) {
	for _, tt := range testProcessEvents {
		w := watcher.NewFakeWatcher()
		w.Add(".")
		store := &metrics.Store{}
		lines := make(chan string)
		fs := &afero.MemMapFs{}
		l := newLoaderWithFs(w, store, lines, fs)
		for i := range tt.events {
			e := tt.events[i]
			switch e := e.(type) {
			case watcher.CreateEvent:
				if e.Pathname != "notexist.mtail" {
					_, err := fs.Create(e.Pathname)
					if err != nil {
						t.Fatalf("Create failed for %s: %s", e.Pathname, err)
					}
				}
				w.InjectCreate(e.Pathname)
			case watcher.DeleteEvent:
				err := fs.Remove(e.Pathname)
				if err != nil {
					t.Fatalf("Remove failed for %s: %s", e.Pathname, err)
				}
				w.InjectDelete(e.Pathname)
			case watcher.UpdateEvent:
				if e.Pathname != "notexist.mtail" {
					f, err := fs.Open(e.Pathname)
					if err != nil {
						t.Fatalf("Couldn't open file %s for test: %s", e.Pathname, err)
					}
					_, err = f.WriteString(testProgram)
					if err != nil {
						t.Fatalf("Couldn't write file contents: %s", err)
					}
					if err = f.Close(); err != nil {
						t.Fatalf("Close failed: %s", err)
					}
				}
				w.InjectUpdate(e.Pathname)
			}
		}
		// ugh; figure out something to synchronise after LoadProg
		time.Sleep(10 * time.Millisecond)
		programs := make([]string, 0)
		l.handleMu.RLock()
		for program := range l.handles {
			programs = append(programs, program)
		}
		l.handleMu.RUnlock()
		l.handleMu.RLock()
		if diff := pretty.Compare(programs, tt.expectedPrograms); len(diff) > 0 {
			t.Errorf("%s: loaded programs don't match. l.handles: %+#v\n%s", tt.name, l.handles, diff)
		}
		l.handleMu.RUnlock()
		close(lines)

	}
}
