// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"os"
	"path"
	"strings"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

func TestNewLoader(t *testing.T) {
	w := watcher.NewFakeWatcher()
	store := metrics.NewStore()
	inLines := make(chan *logline.LogLine)
	l, err := NewLoader("", store, inLines, w)
	if err != nil {
		t.Fatalf("couldn't create loader: %s", err)
	}
	done := make(chan struct{})
	outLines := make(chan *logline.LogLine)
	handle := &vmHandle{outLines, done}
	l.handleMu.Lock()
	l.handles["test"] = handle
	l.handleMu.Unlock()
	go func() {
		for range outLines {
		}
		close(done)
	}()
	close(inLines)
	<-outLines
}

func TestCompileAndRun(t *testing.T) {
	var testProgram = "/$/ {}\n"
	store := metrics.NewStore()
	lines := make(chan *logline.LogLine)
	w := watcher.NewFakeWatcher()
	l, err := NewLoader("", store, lines, w)
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
			{watcher.Create, "foo.mtail"},
			{watcher.Update, "foo.mtail"}},
		[]string{"foo.mtail"}},
	{"unload",
		[]watcher.Event{
			{watcher.Create, "foo.mtail"},
			{watcher.Update, "foo.mtail"},
			{watcher.Delete, "foo.mtail"}},
		[]string{}},
	{"reload",
		[]watcher.Event{
			{watcher.Create, "foo.mtail"},
			{watcher.Update, "foo.mtail"},
			{watcher.Update, "foo.mtail"}},
		[]string{"foo.mtail"}},
	{"bad extension",
		[]watcher.Event{
			{watcher.Create, "foo.mtail.dpkg-dist"},
			{watcher.Update, "foo.mtail.dpkg-dist"}},
		[]string{}},
	{"not exist",
		[]watcher.Event{
			{watcher.Create, "notexist.mtail"},
			{watcher.Update, "notexist.mtail"}},
		[]string{}},
}

var testProgram = "/$/ {}\n"

func TestProcessEvents(t *testing.T) {
	for _, tt := range testProcessEvents {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			w := watcher.NewFakeWatcher()
			store := metrics.NewStore()
			lines := make(chan *logline.LogLine)
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()

			l, err := NewLoader(tmpDir, store, lines, w)
			if err != nil {
				t.Fatalf("couldn't create loader: %s", err)
			}
			testutil.FatalIfErr(t, l.LoadAllPrograms())
			for i := range tt.events {
				e := tt.events[i]
				switch e.Op {
				case watcher.Create:
					if e.Pathname != "notexist.mtail" {
						testutil.TestOpenFile(t, path.Join(tmpDir, e.Pathname))
					}
					w.InjectCreate(path.Join(tmpDir, e.Pathname))
				case watcher.Delete:
					err := os.Remove(path.Join(tmpDir, e.Pathname))
					if err != nil {
						t.Fatalf("Remove failed for %s: %s", e.Pathname, err)
					}
					w.InjectDelete(path.Join(tmpDir, e.Pathname))
				case watcher.Update:
					if e.Pathname != "notexist.mtail" {
						f := testutil.TestOpenFile(t, path.Join(tmpDir, e.Pathname))
						_, err = f.WriteString(testProgram)
						if err != nil {
							t.Fatalf("Couldn't write file contents: %s", err)
						}
						if err = f.Close(); err != nil {
							t.Fatalf("Close failed: %s", err)
						}
					}
					w.InjectUpdate(path.Join(tmpDir, e.Pathname))
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
			if diff := testutil.Diff(tt.expectedPrograms, programs); diff != "" {
				t.Errorf("%q: loaded programs don't match.\nl.handles: %+#v\n%s", tt.name, l.handles, diff)
			}
			l.handleMu.RUnlock()
			close(lines)
		})
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
	inLines := make(chan *logline.LogLine)
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()
	l, err := NewLoader(tmpDir, store, inLines, w)
	if err != nil {
		t.Fatalf("couldn't create loader: %s", err)
	}

	for _, name := range testProgFiles {
		f := testutil.TestOpenFile(t, path.Join(tmpDir, name))
		n, err := f.WriteString(testProgram)
		if err != nil {
			t.Fatal(err)
		}
		glog.Infof("Wrote %d bytes", n)
		err = l.LoadProgram(path.Join(tmpDir, name))
		if err != nil {
			t.Fatalf("couldn't load file: %s error: %s", name, err)
		}
	}
}
