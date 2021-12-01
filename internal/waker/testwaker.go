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

	wakeeReady chan struct{}
	wakeeDone  chan struct{}
	wait       chan struct{}

	mu   sync.Mutex // protects following fields
	wake chan struct{}
}

// WakeFunc describes a function used by tests to trigger a wakeup of blocked idle goroutines under test.  It takes as first parameter the number of goroutines to await before returning to the caller.
type WakeFunc func(int)

// NewTest creates a new Waker to be used in tests, returning it and a function to trigger a wakeup.  The constructor parameter says how many wakees are expected in the first pass.
func NewTest(ctx context.Context, n int) (Waker, WakeFunc) {
	t := &testWaker{
		ctx:        ctx,
		n:          n,
		wakeeReady: make(chan struct{}),
		wakeeDone:  make(chan struct{}),
		wait:       make(chan struct{}),
		wake:       make(chan struct{}),
	}
	initDone := make(chan struct{})
	go func() {
		defer close(initDone)
		for i := 0; i < t.n; i++ {
			<-t.wakeeDone
		}
	}()
	wakeFunc := func(after int) {
		<-initDone
		glog.InfoDepth(1, "TestWaker yielding to Wakee")
		for i := 0; i < t.n; i++ {
			t.wait <- struct{}{}
		}
		glog.Infof("waiting for %d wakees to get the wake chan", t.n)
		for i := 0; i < t.n; i++ {
			<-t.wakeeReady
		}
		t.broadcastWakeAndReset()
		// Now wakeFunc blocks here
		glog.Infof("waiting for %d wakees to return to Wake", after)
		for i := 0; i < after; i++ {
			<-t.wakeeDone
		}
		t.n = after
		glog.InfoDepth(1, "Wakee yielding to TestWaker")
	}
	return t, wakeFunc
}

// Wake satisfies the Waker interface.
func (t *testWaker) Wake() (w <-chan struct{}) {
	t.mu.Lock()
	w = t.wake
	t.mu.Unlock()
	glog.InfoDepth(1, "waiting for wakeup on chan ", w)
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
	glog.Infof("broadcasting wake to chan %p", t.wake)
	close(t.wake)
	t.wake = make(chan struct{})
	glog.Info("wake channel reset")
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
