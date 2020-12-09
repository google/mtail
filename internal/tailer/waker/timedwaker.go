// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker

import (
	"sync"
	"time"
)

// A timedWaker wakes callers on a regular interval.
type timedWaker struct {
	Waker

	t    *time.Ticker
	mu   sync.Mutex // protects following fields
	wake chan struct{}
}

// NewTimed returns a new TimedWaker and a function to call when the timer should be shutdown.
func NewTimed(interval time.Duration) (*timedWaker, func()) {
	t := &timedWaker{
		t:    time.NewTicker(interval),
		wake: make(chan struct{}),
	}
	done := make(chan struct{})
	stopFunc := func() {
		close(done)
	}
	go func() {
		defer t.t.Stop()
		for {
			select {
			case <-t.t.C:
				t.mu.Lock()
				close(t.wake)
				t.wake = make(chan struct{})
				t.mu.Unlock()
			case <-done:
				return
			}
		}
	}()
	return t, stopFunc
}

// Wake implements the Waker interface.
func (t *timedWaker) Wake() (w <-chan struct{}) {
	t.mu.Lock()
	w = t.wake
	t.mu.Unlock()
	return w
}
