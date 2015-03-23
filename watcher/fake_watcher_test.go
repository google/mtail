package watcher

import (
	"sync"
	"testing"
)

func TestFakeWatcher(t *testing.T) {
	w := NewFakeWatcher()
	defer w.Close()

	w.Add("/tmp")
	if _, ok := w.watches["/tmp"]; !ok {
		t.Errorf("Not watching /tmp, w contains: %+#v", w.watches)
	}

	w.Remove("/tmp")
	if _, ok := w.watches["/tmp"]; ok {
		t.Errorf("Still watching /tmp, w contains: %+#v", w.watches)
	}

	w.Add("/tmp")
	wg := sync.WaitGroup{}
	wg.Add(1)
	go func() {
		name := <-w.Creates()
		if name != "/tmp/log" {
			t.Errorf("event doesn't match: %s\n", name)
		}
		wg.Done()
	}()
	w.InjectCreate("/tmp/log")
	wg.Wait()

	w.Add("/tmp/foo")
	wg = sync.WaitGroup{}
	wg.Add(1)
	go func() {
		name := <-w.Updates()
		if name != "/tmp/foo" {
			t.Errorf("event doesn't match name: %s\n", name)
		}
		wg.Done()
	}()
	w.InjectUpdate("/tmp/foo")
	wg.Wait()
}
