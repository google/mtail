// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker

import (
	"context"
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

// NewTimed returns a new timedWaker that is shut down when the context is cancelled.
func NewTimed(ctx context.Context, interval time.Duration) Waker {
	t := &timedWaker{
		t:    time.NewTicker(interval),
		wake: make(chan struct{}),
	}
	go func() {
		defer t.t.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-t.t.C:
				t.mu.Lock()
				close(t.wake)
				t.wake = make(chan struct{})
				t.mu.Unlock()
			}
		}
	}()
	return t
}

// Wake implements the Waker interface.
func (t *timedWaker) Wake() (w <-chan struct{}) {
	t.mu.Lock()
	w = t.wake
	t.mu.Unlock()
	return w
}
