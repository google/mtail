// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker

import (
	"sync"

	"github.com/golang/glog"
)

// A testWaker is used to manually signal to idle routines it's time to look for new work.
// It can be configured to count how many routines have called Wake before it will wake them.
// A testWaker can only be woken once, however.
type testWaker struct {
	Waker

	wg sync.WaitGroup // counting how many wakees have asked to be woken

	mu   sync.Mutex // protects following fields
	wake chan struct{}
}

// NewTest creates a new Waker to be used in tests, returning it and a function to that when called will wake all listeners.  If n > 0, the wake won't be sent until n calls to Wake have been received.
func NewTest(n int) (*testWaker, func()) {
	t := &testWaker{
		wake: make(chan struct{}),
	}
	t.wg.Add(n)
	wakeFunc := func() {
		glog.Infof("waiting for wakees")
		t.wg.Wait()
		t.mu.Lock()
		glog.Infof("closing")
		close(t.wake)
		t.wake = make(chan struct{})
		glog.Infof("reset")
		t.mu.Unlock()
		t.wg.Add(n)
	}
	return t, wakeFunc
}

// Wake satisfies the Waker interface
func (t *testWaker) Wake() (w <-chan struct{}) {
	defer t.wg.Done()
	t.mu.Lock()
	w = t.wake
	t.mu.Unlock()
	glog.Infof("got wake chan %p", w)
	return
}
