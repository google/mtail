// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"strings"
	"testing"
	"time"
)

var test_program = "/$/ { }"

func startEmtail(t *testing.T, pathnames []string) {
	// start server
	vms := make([]*vm, 0)
	prog, errors := Compile("test", strings.NewReader(test_program))
	if len(errors) > 0 {
		t.Errorf("Couldn't compile program: %s", errors)
	}
	vms = append(vms, prog)
	lines := make(chan string)
	go RunVms(vms, lines)
	StartEmtail(lines, pathnames)
}

func TestHandleLogUpdates(t *testing.T) {
	if testing.Short() {
		return
	}
	// make temp dir
	workdir, err := ioutil.TempDir("", "emtail_test")
	if err != nil {
		t.Errorf("could not create temporary directory: %s", err)
	}
	defer func() {
		err := os.RemoveAll(workdir)
		if err != nil {
			t.Errorf("Could not remove temp dir: %s", err)
		}
	}()
	// touch log file
	log_filepath := path.Join(workdir, "log")
	log_file, err := os.Create(log_filepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer log_file.Close()
	pathnames := []string{log_filepath}
	startEmtail(t, pathnames)
	ex_lines := []string{"hi", "hi2", "hi3"}
	for i, x := range ex_lines {
		// write to log file
		log_file.WriteString(x + "\n")
		// TODO(jaq): remove slow sleep
		time.Sleep(100 * time.Millisecond)
		// check log line count increase
		expected := fmt.Sprintf("%d", i+1)
		if line_count.String() != expected {
			t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, line_count.String())
		}
	}
}

func TestHandleLogRotation(t *testing.T) {
	if testing.Short() {
		return
	}
	// make temp dir
	workdir, err := ioutil.TempDir("", "emtail_test")
	if err != nil {
		t.Errorf("could not create temporary directory: %s", err)
	}
	defer func() {
		err := os.RemoveAll(workdir)
		if err != nil {
			t.Errorf("Could not remove temp dir: %s", err)
		}
	}()
	log_filepath := path.Join(workdir, "log")
	// touch log file
	log_file, err := os.Create(log_filepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer log_file.Close()
	// Create a logger
	stop := make(chan bool, 1)
	hup := make(chan bool, 1)
	pathnames := []string{log_filepath}
	startEmtail(t, pathnames)
	line_count.Set(0)
	go func() {
		log_file := log_file
		var err error
		i := 0
		running := true
		for running {
			select {
			case <-hup:
				// touch log file
				log_file, err = os.Create(log_filepath)
				if err != nil {
					t.Errorf("could not touch log file: %s", err)
				}
				defer log_file.Close()
			default:
				log_file.WriteString(fmt.Sprintf("%s\n", i))
				time.Sleep(100 * time.Millisecond)
				i++
				if i >= 10 {
					running = false
				}
			}
		}
		stop <- true
	}()
	go func() {
		for {
			select {
			case <-time.After(5 * 100 * time.Millisecond):
				err = os.Rename(log_filepath, log_filepath+".1")
				if err != nil {
					t.Errorf("could not rename log file: %s", err)
				}
				hup <- true
				return
			}
		}
	}()
	<-stop
	expected := "10"
	if line_count.String() != expected {
		t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, line_count.String())
	}
}
