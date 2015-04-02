package watcher

// EventType enumerates the type of event from the watcher.
type EventType int

const (
	Create EventType = iota
	Update
	Delete
)

// Event is a message describing a filesystem event.
type Event struct {
	Pathname string
	Type     EventType
}

type Watcher interface {
	Add(name string) error
	Close() error
	Remove(name string) error
	Events() <-chan Event
}
