// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker

// A Waker is used to signal to idle routines it's time to look for new work.
type Waker interface {
	// Wake returns a channel that's closed when the idle routine should wake up.
	Wake() <-chan struct{}
}
