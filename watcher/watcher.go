// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package watcher provides a way of watching for filesystem events and
// notifying observers when they occur.
package watcher

// Event is a generalisation of events sent from the watcher to its listeners.
type Event interface {
}

// CreateEvent signifies a file was created in a watched directory.
type CreateEvent struct {
	Pathname string
}

// UpdateEvent signifies a watched file was modified.
type UpdateEvent struct {
	Pathname string
}

// DeleteEvent signifies a watched file was deleted.
type DeleteEvent struct {
	Pathname string
}

// Watcher describes an interface for filesystem watching.
type Watcher interface {
	Add(name string) error
	Close() error
	Remove(name string) error
	Events() <-chan Event
}
