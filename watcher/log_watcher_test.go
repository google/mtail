package watcher

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"reflect"
	"testing"
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
		t.Fatalf("coudln't create a watchre: %s\n", err)
	}
	defer w.Close()

	var creates []string
	var updates []string

	done := make(chan bool)
	go func() {
		for {
			select {
			case name, more := <-w.Creates():
				if !more {
					done <- true
					return
				}
				creates = append(creates, name)
			case name, more := <-w.Updates():
				if !more {
					done <- true
				}
				updates = append(updates, name)
			}
		}
	}()

	w.Add(workdir)
	f, err := os.Open(filepath.Join(workdir, "logfile"))
	f.WriteString("hi")
	f.Close()
	os.Remove(filepath.Join(workdir, "logfile"))
	<-done

	expected_creates := []string{workdir + "/logfile"}
	expected_updates := []string{workdir + "/logfile"}
	if !reflect.DeepEqual(expected_creates, creates) {
		t.Errorf("create events not matching\n\texpected: %v\n\treceived: %v", expected_creates, creates)
	}
	if !reflect.DeepEqual(expected_updates, updates) {
		t.Errorf("create events not matching\n\texpected: %v\n\treceived: %v", expected_updates, updates)
	}
}
