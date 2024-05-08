// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker

import (
	"context"
	"sync"

	"github.com/golang/glog"
)

// A testWaker is used to manually signal to idle routines it's time to look for new work.
type testWaker struct {
	Waker

	ctx context.Context

	n int

	name string

	wakeeReady chan struct{}
	wakeeDone  chan struct{}
	wait       chan struct{}

	mu   sync.Mutex // protects following fields
	wake chan struct{}
}

// WakeFunc describes a function used by tests to trigger a wakeup of blocked idle goroutines under test.  It takes as first parameter the number of goroutines to await before returning to the caller.
type WakeFunc func(int)

// NewTest creates a new Waker to be used in tests, returning it and a function to trigger a wakeup.
// `n` says how many wakees are expected to be waiting before the first `wakeFunc` call.
// `name` gives it a name for debug log messages
func NewTest(ctx context.Context, n int, name string) (Waker, WakeFunc) {
	t := &testWaker{
		ctx:        ctx,
		n:          n,
		name:       name,
		wakeeReady: make(chan struct{}),
		wakeeDone:  make(chan struct{}),
		wait:       make(chan struct{}),
		wake:       make(chan struct{}),
	}
	initDone := make(chan struct{})
	go func() {
		defer close(initDone)
		glog.InfoDepthf(1, "TestWaker(%s) waiting for %d wakees", t.name, t.n)
		for i := 0; i < t.n; i++ {
			<-t.wakeeDone
		}
	}()
	// awaken issues a wakeup signal to the "wakees", those clients who've used the `Wake` call.
	awaken := func(after int) {
		<-initDone
		glog.InfoDepthf(1, "TestWaker(%s) yielding to Wakee", t.name)
		for i := 0; i < t.n; i++ {
			t.wait <- struct{}{}
		}
		// First wait for `t.n` wakees to have called `Wake`, synchronising them.
		glog.Infof("TestWaker(%s) waiting for %d wakees to get the wake chan", t.name, t.n)
		for i := 0; i < t.n; i++ {
			<-t.wakeeReady
		}
		t.broadcastWakeAndReset()
		// Now `awaken` blocks here, as we wait for them in turn to return to another call to Wake, in their polling loops.  We wait for only a count of `after` routines this time, as some may exit.
		glog.Infof("TestWaker(%s) waiting for %d wakees to return to Wake", t.name, after)
		for i := 0; i < after; i++ {
			<-t.wakeeDone
		}
		t.n = after
		glog.InfoDepthf(1, "TestWaker(%s): Wakee yielding to TestWaker", t.name)
	}
	return t, awaken
}

// Wake satisfies the Waker interface.
func (t *testWaker) Wake() (w <-chan struct{}) {
	t.mu.Lock()
	w = t.wake
	t.mu.Unlock()
	glog.InfoDepthf(1, "TestWaker(%s) waiting for wakeup on chan %p", t.name, w)
	// Background this so we can return the wake channel.
	// The wakeFunc won't close the channel until this completes.
	go func() {
		// Signal we've reentered Wake.  wakeFunc can't return until we do this.
		select {
		case <-t.ctx.Done():
			return
		case t.wakeeDone <- struct{}{}:
		}
		// Block wakees here until a subsequent wakeFunc is called.
		select {
		case <-t.ctx.Done():
			return
		case <-t.wait:
		}
		// Signal we've got the wake chan, telling wakeFunc it can now issue a broadcast.
		select {
		case <-t.ctx.Done():
			return
		case t.wakeeReady <- struct{}{}:
		}
	}()
	return
}

func (t *testWaker) broadcastWakeAndReset() {
	t.mu.Lock()
	defer t.mu.Unlock()
	glog.Infof("TestWaker(%s) broadcasting wake to chan %p", t.name, t.wake)
	close(t.wake)
	t.wake = make(chan struct{})
	glog.Infof("TestWaker(%s) wake channel reset", t.name)
}

// alwaysWaker never blocks the wakee.
type alwaysWaker struct {
	wake chan struct{}
}

func NewTestAlways() Waker {
	w := &alwaysWaker{
		wake: make(chan struct{}),
	}
	close(w.wake)
	return w
}

func (w *alwaysWaker) Wake() <-chan struct{} {
	return w.wake
}
