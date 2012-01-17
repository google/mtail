// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
        "io/ioutil"
        "os"
        "testing"
)

func TestTail(t *testing.T) {
        d, err := ioutil.TempDir("", "tail_test")
        if err != nil {
                t.Error(err)
        }
        defer os.RemoveAll(d)

        w := NewWatcher()

        logfile := d + "/log"
        f, err := os.Create(logfile)
        if err != nil {
                t.Error(err)
        }
        defer f.Close()

        go w.start()

        ta := NewTailer(w)
        ta.Tail(logfile)

        if _, ok := w.change[logfile]; !ok {
                t.Error("path not found in watcher change channel map")
        }
        w.stop = true
}

func TestHandleLogChange(t *testing.T) {
        d, err := ioutil.TempDir("", "tail_test")
        if err != nil {
                t.Error(err)
        }
        defer os.RemoveAll(d)

        logfile := d + "/log"
        f, err := os.Create(logfile)
        if err != nil {
                t.Error(err)
        }
        defer f.Close()

        ta := NewTailer(nil)
        ta.Tail(logfile)

        _, err = f.WriteString("a\nb\nc\nd\n")
        if err != nil {
                t.Error(err)
        }
        go ta.handleLogChange(logfile)

        for _, expected := range []string{"a", "b", "c", "d"} {
                // Run as a goroutine because it's going to emit lines via output channel
                line := <-ta.Line
                if line != expected {
                        t.Errorf("line doesn't match:\n\texpected: %s\n\tgot: %s", expected, line)
                        continue
                }
        }
}

func TestHandleLogChangePartialLine(t *testing.T) {
        d, err := ioutil.TempDir("", "tail_test")
        if err != nil {
                t.Error(err)
        }
        defer os.RemoveAll(d)

        logfile := d + "/log"
        f, err := os.Create(logfile)
        if err != nil {
                t.Error(err)
        }
        defer f.Close()

        ta := NewTailer(nil)
        ta.Tail(logfile)

        _, err = f.WriteString("a")
        if err != nil {
                t.Error(err)
        }
        go ta.handleLogChange(logfile)
        select {
        case line := <-ta.Line:
                t.Errorf("unexpected line found: %s", line)
        default:
        }

        _, err = f.WriteString("b")
        if err != nil {
                t.Error(err)
        }
        go ta.handleLogChange(logfile)

        select {
        case line := <-ta.Line:
                t.Errorf("unexpected line found: %s", line)
        default:
        }

        _, err = f.WriteString("\n")
        if err != nil {
                t.Error(err)
        }
        go ta.handleLogChange(logfile)
        line := <-ta.Line
        if line != "ab" {
                t.Error("line doesn't match: expected 'ab' vs %s", line)
        }
}
