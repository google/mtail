package watcher

type Watcher interface {
	Add(name string) error
	Close() error
	Remove(name string) error
	Creates() chan string
	Updates() chan string
	Deletes() chan string
}
