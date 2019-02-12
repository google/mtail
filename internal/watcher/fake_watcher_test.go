// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package watcher

import (
	"sync"
	"testing"

	"github.com/google/mtail/internal/testutil"
)

func TestFakeWatcher(t *testing.T) {
	w := NewFakeWatcher()
	defer w.Close()

	handle, eventsChannel := w.Events()

	testutil.FatalIfErr(t, w.Add("/tmp", handle))
	if _, ok := w.watches["/tmp"]; !ok {
		t.Errorf("Not watching /tmp, w contains: %+#v", w.watches)
	}

	testutil.FatalIfErr(t, w.Remove("/tmp"))
	if _, ok := w.watches["/tmp"]; ok {
		t.Errorf("Still watching /tmp, w contains: %+#v", w.watches)
	}

	testutil.FatalIfErr(t, w.Add("/tmp", handle))
	wg := sync.WaitGroup{}
	wg.Add(1)

	go func() {
		e := <-eventsChannel
		switch e.Op {
		case Create:
			if e.Pathname != "/tmp/log" {
				t.Errorf("event doesn't match: %q\n", e)
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
		wg.Done()
	}()
	w.InjectCreate("/tmp/log")
	wg.Wait()

	testutil.FatalIfErr(t, w.Add("/tmp/foo", handle))
	wg = sync.WaitGroup{}
	wg.Add(1)
	go func() {
		e := <-eventsChannel
		switch e.Op {
		case Update:
			if e.Pathname != "/tmp/foo" {
				t.Errorf("event doesn't match name: %q\n", e)
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
		wg.Done()
	}()
	w.InjectUpdate("/tmp/foo")
	wg.Wait()

	wg = sync.WaitGroup{}
	wg.Add(1)
	go func() {
		e := <-eventsChannel
		switch e.Op {
		case Delete:
			if e.Pathname != "/tmp/foo" {
				t.Errorf("event doesn't match name: %q\n", e)
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
		wg.Done()
	}()
	w.InjectDelete("/tmp/foo")
	wg.Wait()
}

func TestFakeWatcherUnwatchedFiles(t *testing.T) {
	w := NewFakeWatcher()
	wg := sync.WaitGroup{}
	wg.Add(1)
	_, eventsChannel := w.Events()
	go func() {
		for e := range eventsChannel {
			switch e.Op {
			case Create, Delete, Update:
				t.Errorf("Received an event, expecting nothing: %q", e)
			default:
			}
		}
		wg.Done()
	}()
	w.InjectCreate("/tmp/log")
	w.Close()
	wg.Wait()

	w = NewFakeWatcher()
	wg = sync.WaitGroup{}
	wg.Add(1)
	go func() {
		for e := range eventsChannel {
			switch e.Op {
			case Create, Delete, Update:
				t.Errorf("Received an event, expecting nothing: %q", e)
			default:
			}
		}
		wg.Done()
	}()
	w.InjectUpdate("/tmp/foo")
	w.Close()
	wg.Wait()

	w = NewFakeWatcher()
	wg = sync.WaitGroup{}
	wg.Add(1)
	go func() {
		for e := range eventsChannel {
			switch e.Op {
			case Create, Delete, Update:
				t.Errorf("Received an event, expecting nothing: %q", e)
			default:
			}
		}
		wg.Done()
	}()
	w.InjectDelete("/tmp/foo")
	w.Close()
	wg.Wait()
}

func TestNoSuchHandle(t *testing.T) {
	w := NewFakeWatcher()
	err := w.Add("foo", 1)
	if err == nil {
		t.Error("expecting error, got nil")
	}
}
