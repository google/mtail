// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"

	"github.com/go-test/deep"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/watcher"
	"github.com/spf13/afero"
)

func TestNewLoader(t *testing.T) {
	w := watcher.NewFakeWatcher()
	store := metrics.NewStore()
	inLines := make(chan *tailer.LogLine)
	fs := afero.NewMemMapFs()
	o := LoaderOptions{store, inLines, w, fs, false, false, false, false, true}
	l, err := NewLoader(o)
	if err != nil {
		t.Fatalf("couldn't create loader: %s", err)
	}
	done := make(chan struct{})
	outLines := make(chan *tailer.LogLine)
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
	var testProgram = "/$/ {}\n"
	store := metrics.NewStore()
	lines := make(chan *tailer.LogLine)
	w := watcher.NewFakeWatcher()
	fs := afero.NewMemMapFs()
	o := LoaderOptions{store, lines, w, fs, false, false, false, false, true}
	l, err := NewLoader(o)
	if err != nil {
		t.Fatalf("couldn't create loader: %s", err)
	}
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
			watcher.CreateEvent{Pathname: "foo.mtail"},
			watcher.UpdateEvent{Pathname: "foo.mtail"}},
		[]string{"foo.mtail"}},
	{"unload",
		[]watcher.Event{
			watcher.CreateEvent{Pathname: "foo.mtail"},
			watcher.UpdateEvent{Pathname: "foo.mtail"},
			watcher.DeleteEvent{Pathname: "foo.mtail"}},
		[]string{}},
	{"reload",
		[]watcher.Event{
			watcher.CreateEvent{Pathname: "foo.mtail"},
			watcher.UpdateEvent{Pathname: "foo.mtail"},
			watcher.UpdateEvent{Pathname: "foo.mtail"}},
		[]string{"foo.mtail"}},
	{"bad extension",
		[]watcher.Event{
			watcher.CreateEvent{Pathname: "foo.mtail.dpkg-dist"},
			watcher.UpdateEvent{Pathname: "foo.mtail.dpkg-dist"}},
		[]string{}},
	{"not exist",
		[]watcher.Event{
			watcher.CreateEvent{Pathname: "notexist.mtail"},
			watcher.UpdateEvent{Pathname: "notexist.mtail"}},
		[]string{}},
}

var testProgram = "/$/ {}\n"

func TestProcessEvents(t *testing.T) {
	for _, tt := range testProcessEvents {
		t.Logf("Starting %s", tt.name)
		w := watcher.NewFakeWatcher()
		w.Add(".")
		store := metrics.NewStore()
		lines := make(chan *tailer.LogLine)
		fs := afero.NewMemMapFs()
		o := LoaderOptions{store, lines, w, fs, false, false, false, false, true}
		l, err := NewLoader(o)
		if err != nil {
			t.Fatalf("couldn't create loader: %s", err)
		}
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
					f, err := fs.Create(e.Pathname)
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
		w.Close()
		<-l.watcherDone
		l.handleMu.RLock()
		programs := make([]string, 0)
		for program := range l.handles {
			programs = append(programs, program)
		}
		l.handleMu.RUnlock()
		l.handleMu.RLock()
		if diff := deep.Equal(tt.expectedPrograms, programs); diff != nil {
			t.Errorf("%q: loaded programs don't match.\nl.handles: %+#v\n%s", tt.name, l.handles, diff)
		}
		l.handleMu.RUnlock()
		close(lines)

	}
}

var testProgFiles = []string{
	"test.wrongext",
	"test.mtail",
	".test",
}

func TestLoadProg(t *testing.T) {
	w := watcher.NewFakeWatcher()
	store := metrics.NewStore()
	inLines := make(chan *tailer.LogLine)
	fs := afero.NewMemMapFs()
	o := LoaderOptions{store, inLines, w, fs, false, false, false, false, true}
	l, err := NewLoader(o)
	if err != nil {
		t.Fatalf("couldn't create loader: %s", err)
	}

	for _, f := range testProgFiles {
		afero.WriteFile(fs, f, []byte(testProgram), 0644)
		err = l.LoadProg(f)
		if err != nil {
			t.Fatalf("couldn't load file: %s error: %s", f, err)
		}
	}
}
