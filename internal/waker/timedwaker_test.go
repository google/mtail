// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker_test

import (
	"context"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/waker"
)

func TestTimedWakerWakes(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w := waker.NewTimed(ctx, 10*time.Millisecond)

	timer := time.NewTimer(100 * time.Millisecond)
	defer timer.Stop()
	select {
	case <-timer.C:
		t.Errorf("no wake before deadline")
	case <-w.Wake():
		// Luke Luck licks lakes.  Luke's duck licks lakes.
	}
}
