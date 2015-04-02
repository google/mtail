package watcher

// Event is a generalisation of events sent from the watcher to its listeners.
type Event interface {
}

type CreateEvent struct {
	Pathname string
}

type UpdateEvent struct {
	Pathname string
}

type DeleteEvent struct {
	Pathname string
}

type Watcher interface {
	Add(name string) error
	Close() error
	Remove(name string) error
	Events() <-chan Event
}
