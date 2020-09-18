// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package watcher provides a way of watching for filesystem events and
// notifying observers when they occur.
package watcher

import "context"

type OpType int

const (
	_ OpType = iota
	Create
	Update
	Delete
)

// Event is a generalisation of events sent from the watcher to its listeners.
type Event struct {
	Op       OpType
	Pathname string
}

// Watcher describes an interface for filesystem watching.
type Watcher interface {
	Observe(name string, processor Processor) error
	Unobserve(name string, processor Processor) error
	Poll()
	Close() error
}

// Processor describes an interface for receiving watcher.Events
type Processor interface {
	ProcessFileEvent(context.Context, Event)
}
