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
		e := <-w.Events()
		if e.Type != Create || e.Pathname != "/tmp/log" {
			t.Errorf("event doesn't match: %s\n", e)
		}
		wg.Done()
	}()
	w.InjectCreate("/tmp/log")
	wg.Wait()

	w.Add("/tmp/foo")
	wg = sync.WaitGroup{}
	wg.Add(1)
	go func() {
		e := <-w.Events()
		if e.Type != Update || e.Pathname != "/tmp/foo" {
			t.Errorf("event doesn't match name: %s\n", e)
		}
		wg.Done()
	}()
	w.InjectUpdate("/tmp/foo")
	wg.Wait()
}
