// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker_test

import (
	"testing"
	"time"

	"github.com/google/mtail/internal/waker"
)

func TestTimedWakerWakes(t *testing.T) {
	w, cancel := waker.NewTimed(10 * time.Millisecond)
	defer cancel()

	timer := time.NewTimer(100 * time.Millisecond)
	defer timer.Stop()
	select {
	case <-timer.C:
		t.Errorf("no wake before deadline")
	case <-w.Wake():
		// Luke Luck licks lakes.  Luke's duck licks lakes.
	}
}
