// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"context"
	"os"
	"path"
	"strings"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

func TestNewLoader(t *testing.T) {
	w := watcher.NewFakeWatcher()
	store := metrics.NewStore()
	_, err := NewLoader("", store, w)
	testutil.FatalIfErr(t, err)
}

func TestCompileAndRun(t *testing.T) {
	var testProgram = "/$/ {}\n"
	store := metrics.NewStore()
	w := watcher.NewFakeWatcher()
	l, err := NewLoader("", store, w)
	testutil.FatalIfErr(t, err)
	if err := l.CompileAndRun("Test", strings.NewReader(testProgram)); err != nil {
		t.Errorf("CompileAndRun returned error: %s", err)
	}
	l.handleMu.Lock()
	if len(l.handles) < 1 {
		t.Errorf("no vm handles: %v", l.handles)
	}
	l.handleMu.Unlock()
	l.handleMu.Lock()
	h := l.handles["Test"]
	if h == nil {
		t.Errorf("No handle for Test: %v", l.handles)
	}
	l.handleMu.Unlock()
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

func TestProcessFileEvent(t *testing.T) {
	for _, tt := range testProcessEvents {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			w := watcher.NewFakeWatcher()
			store := metrics.NewStore()
			tmpDir, rmTmpDir := testutil.TestTempDir(t)
			defer rmTmpDir()

			l, err := NewLoader(tmpDir, store, w)
			testutil.FatalIfErr(t, err)
			testutil.FatalIfErr(t, l.LoadAllPrograms())
			for i := range tt.events {
				e := tt.events[i]
				switch e.Op {
				case watcher.Create:
					if e.Pathname != "notexist.mtail" {
						testutil.TestOpenFile(t, path.Join(tmpDir, e.Pathname))
					}
				case watcher.Delete:
					err := os.Remove(path.Join(tmpDir, e.Pathname))
					testutil.FatalIfErr(t, err)
				case watcher.Update:
					if e.Pathname != "notexist.mtail" {
						f := testutil.TestOpenFile(t, path.Join(tmpDir, e.Pathname))
						_, err = f.WriteString(testProgram)
						testutil.FatalIfErr(t, err)
						err := f.Close()
						testutil.FatalIfErr(t, err)
					}
				}
				l.ProcessFileEvent(context.Background(), watcher.Event{e.Op, path.Join(tmpDir, e.Pathname)})
			}
			w.Close()
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
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()
	l, err := NewLoader(tmpDir, store, w)
	testutil.FatalIfErr(t, err)

	for _, name := range testProgFiles {
		f := testutil.TestOpenFile(t, path.Join(tmpDir, name))
		n, err := f.WriteString(testProgram)
		testutil.FatalIfErr(t, err)
		glog.Infof("Wrote %d bytes", n)
		err = l.LoadProgram(path.Join(tmpDir, name))
		testutil.FatalIfErr(t, err)
	}
}
