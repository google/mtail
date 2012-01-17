// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"io/ioutil"
	"os"
	"testing"
)

func TestWatchFile(t *testing.T) {
	d, err := ioutil.TempDir("", "watcher_test")
	if err != nil {
		t.Error(err)
	}
	defer os.RemoveAll(d)

	logfile := d + "/log"
	f, err := os.Create(logfile)
	if err != nil {
		t.Error(err)
	}

	w := NewWatcher()

	change := make(chan string)
	w.WatchLogFile(logfile, change, nil)

	go w.start()

	f.WriteString("hi1\n")
	f.Sync()

	defer f.Close()

	select {
	case <-change:
	default:
		t.Error("no change seen")
	}
	w.stop = true
}
