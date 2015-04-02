package watcher

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestLogWatcher(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping log watcher test in short mode")
	}

	workdir, err := ioutil.TempDir("", "log_watcher_test")
	if err != nil {
		t.Fatalf("could not create temporary working directory: %s", err)
	}

	defer func() {
		err := os.RemoveAll(workdir)
		if err != nil {
			t.Fatalf("could not remove temp dir %s: %s:", workdir, err)
		}
	}()

	w, err := NewLogWatcher()
	if err != nil {
		t.Fatalf("couldn't create a watcher: %s\n", err)
	}
	defer w.Close()

	w.Add(workdir)
	f, err := os.Create(filepath.Join(workdir, "logfile"))
	if err != nil {
		t.Fatalf("couldn't make a logfile in temp dir: %s\n", err)
	}
	select {
	case e := <-w.Events():
		switch e := e.(type) {
		case CreateEvent:
			if e.Pathname != filepath.Join(workdir, "logfile") {
				t.Errorf("create doesn't match")
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
	case <-time.After(10 * time.Millisecond):
		t.Errorf("didn't receive create message")
	}
	f.WriteString("hi")
	f.Close()
	select {
	case e := <-w.Events():
		switch e := e.(type) {
		case UpdateEvent:
			if e.Pathname != filepath.Join(workdir, "logfile") {
				t.Errorf("update doesn't match")
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
	case <-time.After(10 * time.Millisecond):
		t.Errorf("didn't receive update message")
	}
	os.Remove(filepath.Join(workdir, "logfile"))
	select {
	case e := <-w.Events():
		switch e := e.(type) {
		case DeleteEvent:
			if e.Pathname != filepath.Join(workdir, "logfile") {
				t.Errorf("delete doesn't match")
			}
		default:
			t.Errorf("Wrong event type: %q", e)
		}
	case <-time.After(10 * time.Millisecond):
		t.Errorf("didn't receive delete message")
	}
}
