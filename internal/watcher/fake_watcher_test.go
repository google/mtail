// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"context"
	"testing"

	"github.com/google/mtail/internal/testutil"
)

type stubProcessor struct {
	Events []Event
}

func (s *stubProcessor) ProcessFileEvent(ctx context.Context, event Event) {
	s.Events = append(s.Events, event)
}

func TestFakeWatcher(t *testing.T) {
	w := NewFakeWatcher()
	defer w.Close()

	s := &stubProcessor{}

	testutil.FatalIfErr(t, w.Observe("/tmp", s))
	if _, ok := w.watches["/tmp"]; !ok {
		t.Errorf("Not watching /tmp, w contains: %+#v", w.watches)
	}

	testutil.FatalIfErr(t, w.Unobserve("/tmp", s))
	if _, ok := w.watches["/tmp"]; ok {
		t.Errorf("Still watching /tmp, w contains: %+#v", w.watches)
	}

	testutil.FatalIfErr(t, w.Observe("/tmp", s))
	w.InjectCreate("/tmp/log")
	if len(s.Events) != 1 {
		t.Errorf("No event received")
	}
	if s.Events[0].Pathname != "/tmp/log" {
		t.Errorf("event doesn't match: %q\n", s.Events[0])
	}
	if s.Events[0].Op != Create {
		t.Errorf("Wrong event type; %q", s.Events[0])
	}

	testutil.FatalIfErr(t, w.Observe("/tmp/foo", s))
	w.InjectUpdate("/tmp/foo")
	if len(s.Events) != 2 {
		t.Errorf("no even treceived")
	}
	if s.Events[1].Pathname != "/tmp/foo" {
		t.Errorf("event doesn't match name: %q\n", s.Events[1])
	}
	if s.Events[1].Op != Update {
		t.Errorf("Wrong event type: %q", s.Events[1])
	}

	w.InjectDelete("/tmp/foo")
	if len(s.Events) != 3 {
		t.Errorf("no event received")
	}
	if s.Events[2].Pathname != "/tmp/foo" {
		t.Errorf("event doesn't match name: %q\n", s.Events[2])
	}
	if s.Events[2].Op != Delete {
		t.Errorf("Wrong event type: %q", s.Events[2])
	}
}

func TestFakeWatcherUnwatchedFiles(t *testing.T) {
	w := NewFakeWatcher()
	s := &stubProcessor{}
	w.InjectCreate("/tmp/log")
	w.Close()
	if len(s.Events) > 0 {
		t.Errorf("Received an event, expecting nothing: %q", s.Events)
	}

	w = NewFakeWatcher()
	w.InjectUpdate("/tmp/foo")
	w.Close()
	if len(s.Events) > 0 {
		t.Errorf("Received an event, expecting nothing: %q", s.Events)
	}

	w = NewFakeWatcher()
	w.InjectDelete("/tmp/foo")
	w.Close()
	if len(s.Events) > 0 {
		t.Errorf("Received an event, expecting nothing: %q", s.Events)
	}
}
