// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker

import (
	"context"
	"sync"

	"github.com/golang/glog"
)

// A testWaker is used to manually signal to idle routines it's time to look
// for new work.  It works by synchronising several client goroutines
// ("wakees"), waiting for a certain number to have called `Wake()`, and then
// sending them a wakeup signal together.  It then waits in a loop for more
// goroutines to call `Wake` again before returning to the test.
type testWaker struct {
	Waker

	ctx context.Context

	name string

	wakeeReady chan struct{}
	wakeeDone  chan struct{}
	waiting    chan struct{}

	mu   sync.Mutex // protects following fields
	wake chan struct{}
}

// WakeFunc describes a function used by tests to trigger a wakeup of blocked
// idle goroutines under test.  It takes as first parameter the number of
// goroutines to wake up, and the second parameter is the number of goroutines
// to wait to call Wake before returning.
type WakeFunc func(int, int)

// NewTest creates a new Waker to be used in tests, returning it and a function to trigger a wakeup.
// `wait` says how many wakees are expected to be waiting before the first `wakeFunc` call.
// `name` gives it a name for debug log messages
func NewTest(ctx context.Context, wait int, name string) (Waker, WakeFunc) {
	t := &testWaker{
		ctx:        ctx,
		name:       name,
		wakeeReady: make(chan struct{}),
		wakeeDone:  make(chan struct{}),
		waiting:    make(chan struct{}),
		wake:       make(chan struct{}),
	}
	initDone := make(chan struct{})
	go func() {
		defer close(initDone)
		glog.Infof("TestWaker(%s) init waiting for %d wakees to call Wake()", t.name, wait)
		for i := 0; i < wait; i++ {
			<-t.wakeeDone
		}
	}()
	// awaken issues a wakeup signal to the "wakees", those clients who've used
	// the `Wake` call.  wake is the number of wakees we expect to wake up,
	// wait is the number of wakees to wait for before returning.
	awaken := func(wake, wait int) {
		<-initDone
		glog.InfoDepthf(1, "TestWaker(%s) yielding to Wakee", t.name)
		for i := 0; i < wake; i++ {
			t.waiting <- struct{}{}
		}
		// First wait for `t.n` wakees to have called `Wake`, synchronising them.
		glog.InfoDepthf(1, "TestWaker(%s) waiting for %d wakees to receive from the wake chan", t.name, wake)
		for i := 0; i < wake; i++ {
			<-t.wakeeReady
		}
		t.broadcastWakeAndReset()
		// Now `awaken` blocks here, as we wait for them in turn to return to another call to Wake, in their polling loops.  We wait for only a count of `after` routines this time, as some may exit.
		glog.InfoDepthf(1, "TestWaker(%s) waiting for %d wakees to call Wake()", t.name, wait)
		for i := 0; i < wait; i++ {
			<-t.wakeeDone
		}
		glog.InfoDepthf(1, "TestWaker(%s): Wakees returned, yielding to TestWaker", t.name)
	}
	return t, awaken
}

// Wake satisfies the Waker interface.
func (t *testWaker) Wake() (w <-chan struct{}) {
	t.mu.Lock()
	w = t.wake
	t.mu.Unlock()
	glog.InfoDepthf(1, "Wakee on TestWaker(%s) waiting for wakeup on chan %p", t.name, w)
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
		case <-t.waiting:
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
	glog.Infof("TestWaker(%s) wake channel reset to chan %p", t.name, t.wake)
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
