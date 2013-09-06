// Copyright 2013 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"exp/inotify"
	"reflect"
	"testing"
)

type fakewatcher struct {
	inotify.Watcher
}

func (f fakewatcher) AddWatch(string, uint32) error { return nil }
func (f fakewatcher) Close() error                  { return nil }
func (f fakewatcher) RemoveWatch(string) error      { return nil }
func (f fakewatcher) Watch(string) error            { return nil }
func (f fakewatcher) Errors() chan error            { return f.Error }
func (f fakewatcher) Events() chan *inotify.Event   { return f.Event }

var progloadertests = []struct {
	*inotify.Event
	pathnames []string
}{
	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "foo.em"},
		[]string{"foo.em"},
	},
	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "foo.em"},
		[]string{"foo.em"},
	},
	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "bar.em"},
		[]string{"foo.em", "bar.em"},
	},
	{
		&inotify.Event{Mask: inotify.IN_MODIFY,
			Cookie: 0,
			Name:   "bar.em"},
		[]string{"foo.em", "bar.em"},
	},

	{
		&inotify.Event{Mask: inotify.IN_CREATE,
			Cookie: 0,
			Name:   "no.gz"},
		[]string{"foo.em", "bar.em"},
	},
}

func TestProgLoader(t *testing.T) {
	var fake fakewatcher
	fake.Error = make(chan error)
	fake.Event = make(chan *inotify.Event)
	l := NewProgLoader(fake)
	go l.start()
	for _, tt := range progloadertests {
		fake.Event <- tt.Event
		pathnames := make(map[string]struct{})
		for _, p := range tt.pathnames {
			pathnames[p] = struct{}{}
		}
		if !reflect.DeepEqual(pathnames, l.pathnames) {
			t.Errorf("Pathnames don't match.\n\texpected %q\n\treceived %q", pathnames, l.pathnames)
		}
	}
}

// func TestTail(t *testing.T) {
// 	d, err := ioutil.TempDir("", "tail_test")
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	defer os.RemoveAll(d)

// 	logfile := d + "/log"
// 	f, err := os.Create(logfile)
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	defer f.Close()

// 	lines := make(chan string)
// 	ta := NewTailer(lines)
// 	if ta == nil {
// 		t.Fatalf("Couldn't make a tailer.")
// 	}
// 	defer ta.Stop()
// 	ta.Tail(logfile)

// 	if _, ok := ta.files[logfile]; !ok {
// 		t.Error("path not found in files map")
// 	}
// }

// func TestHandleLogChange(t *testing.T) {
// 	d, err := ioutil.TempDir("", "tail_test")
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	defer os.RemoveAll(d)

// 	logfile := d + "/log"
// 	f, err := os.Create(logfile)
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	defer f.Close()

// 	lines := make(chan string)
// 	ta := NewTailer(lines)
// 	if ta == nil {
// 		t.Fatalf("Couldn't make a tailer.")
// 	}
// 	defer ta.Stop()
// 	ta.Tail(logfile)

// 	_, err = f.WriteString("a\nb\nc\nd\n")
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	go ta.handleLogUpdate(logfile)

// 	for _, expected := range []string{"a", "b", "c", "d"} {
// 		// Run as a goroutine because it's going to emit lines via output channel
// 		line := <-ta.lines
// 		if line != expected {
// 			t.Errorf("line doesn't match:\n\texpected: %s\n\tgot: %s", expected, line)
// 			continue
// 		}
// 	}
// }

// func TestHandleLogChangePartialLine(t *testing.T) {
// 	d, err := ioutil.TempDir("", "tail_test")
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	defer os.RemoveAll(d)

// 	logfile := d + "/log"
// 	f, err := os.Create(logfile)
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	defer f.Close()

// 	lines := make(chan string)
// 	ta := NewTailer(lines)
// 	if ta == nil {
// 		t.Fatalf("Couldn't make a tailer.")
// 	}
// 	defer ta.Stop()
// 	ta.Tail(logfile)

// 	_, err = f.WriteString("a")
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	go ta.handleLogUpdate(logfile)
// 	select {
// 	case line := <-ta.lines:
// 		t.Errorf("unexpected line found: %s", line)
// 	default:
// 	}

// 	_, err = f.WriteString("b")
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	go ta.handleLogUpdate(logfile)

// 	select {
// 	case line := <-ta.lines:
// 		t.Errorf("unexpected line found: %s", line)
// 	default:
// 	}

// 	_, err = f.WriteString("\n")
// 	if err != nil {
// 		t.Error(err)
// 	}
// 	go ta.handleLogUpdate(logfile)
// 	line := <-ta.lines
// 	if line != "ab" {
// 		t.Errorf("line doesn't match: expected 'ab' vs %s", line)
// 	}
// }
