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

func startEmtail(t *testing.T, log_pathnames []string, prog_pathname string) {
	w, err := NewInotifyWatcher()
	if err != nil {
		t.Errorf("Couldn't create watcher:", err)
	}
	p := NewProgLoader(w)
	// start server
	prog, errors := Compile("test", strings.NewReader(test_program))
	if len(errors) > 0 {
		t.Errorf("Couldn't compile program: %s", errors)
	}
	p.e.addVm("test", prog)
	if prog_pathname != "" {
		p.LoadProgs(prog_pathname)
	}
	lines := make(chan string)
	line_count.Set(0)
	go p.e.run(lines)
	StartEmtail(lines, log_pathnames)
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
	startEmtail(t, pathnames, "")
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
	startEmtail(t, pathnames, "")

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

func TestHandleNewLogAfterStart(t *testing.T) {
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
	// Start up emtail
	log_filepath := path.Join(workdir, "log")
	pathnames := []string{log_filepath}
	startEmtail(t, pathnames, "")

	// touch log file
	log_file, err := os.Create(log_filepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer log_file.Close()
	ex_lines := []string{"hi", "hi2", "hi3"}
	for _, x := range ex_lines {
		// write to log file
		log_file.WriteString(x + "\n")
	}
	// TODO(jaq): remove slow sleep
	time.Sleep(100 * time.Millisecond)
	// check log line count increase
	expected := fmt.Sprintf("%d", len(ex_lines))
	if line_count.String() != expected {
		t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, line_count.String())
	}
}

func TestHandleNewLogIgnored(t *testing.T) {
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
	// Start emtail
	log_filepath := path.Join(workdir, "log")
	pathnames := []string{log_filepath}
	startEmtail(t, pathnames, "")

	// touch log file
	new_log_filepath := path.Join(workdir, "log1")

	log_file, err := os.Create(new_log_filepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer log_file.Close()
	expected := "0"
	if line_count.String() != expected {
		t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, line_count.String())
	}
}

func makeTempDir(t *testing.T) (workdir string) {
	var err error
	if workdir, err = ioutil.TempDir("", "emtail_test"); err != nil {
		t.Errorf("ioutil.TempDir failed: %s", err)
	}
	return
}

func removeTempDir(t *testing.T, workdir string) {
	if err := os.RemoveAll(workdir); err != nil {
		t.Errorf("os.RemoveAll failed: %s", err)
	}
}

// TODO(jaq): The sleeps in here are racy.  What can we use to sync through inotify?
func TestHandleNewProgram(t *testing.T) {
	if testing.Short() {
		return
	}

	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)

	startEmtail(t, []string{}, workdir)

	expected_prog_loads := "{}"
	if prog_loads.String() != expected_prog_loads {
		t.Errorf("Prog loads not same\n\texpected: %s\n\trecevied: %s", expected_prog_loads, prog_loads.String())
	}

	prog_path := path.Join(workdir, "prog.em")
	prog_file, err := os.Create(prog_path)
	if err != nil {
		t.Errorf("prog create failed: %s", err)
	}
	prog_file.WriteString("/$/ {}\n")
	prog_file.Close()

	// Wait for inotify
	time.Sleep(100 * time.Millisecond)
	expected_prog_loads = `{"prog.em": 1}`
	if prog_loads.String() != expected_prog_loads {
		t.Errorf("Prog loads not same\n\texpected: %s\n\trecevied: %s", expected_prog_loads, prog_loads.String())
	}

	bad_prog_path := path.Join(workdir, "prog.em.dpkg-dist")
	bad_prog_file, err := os.Create(bad_prog_path)
	if err != nil {
		t.Errorf("prog create failed: %s", err)
	}
	bad_prog_file.WriteString("/$/ {}\n")
	bad_prog_file.Close()

	time.Sleep(100 * time.Millisecond)
	expected_prog_loads = `{"prog.em": 1}`
	if prog_loads.String() != expected_prog_loads {
		t.Errorf("Prog loads not same\n\texpected: %s\n\trecevied: %s", expected_prog_loads, prog_loads.String())
	}
	expected_prog_errs := `{}`
	if prog_load_errors.String() != expected_prog_errs {
		t.Errorf("Prog errors not same\n\texpected: %s\n\treceived: %s", expected_prog_errs, prog_load_errors.String())
	}

	os.Rename(bad_prog_path, prog_path)
	time.Sleep(100 * time.Millisecond)
	expected_prog_loads = `{"prog.em": 2}`
	if prog_loads.String() != expected_prog_loads {
		t.Errorf("Prog loads not same\n\texpected: %s\n\trecevied: %s", expected_prog_loads, prog_loads.String())
	}
	expected_prog_errs = `{}`
	if prog_load_errors.String() != expected_prog_errs {
		t.Errorf("Prog errors not same\n\texpected: %s\n\treceived: %s", expected_prog_errs, prog_load_errors.String())
	}

	broken_prog_path := path.Join(workdir, "broken.em")
	broken_prog_file, err := os.Create(broken_prog_path)
	if err != nil {
		t.Errorf("prog create failed: %s", err)
	}
	broken_prog_file.WriteString("?\n")
	broken_prog_file.Close()

	time.Sleep(100 * time.Millisecond)

	expected_prog_loads = `{"prog.em": 2}`
	if prog_loads.String() != expected_prog_loads {
		t.Errorf("Prog loads not same\n\texpected: %s\n\trecevied: %s", expected_prog_loads, prog_loads.String())
	}
	expected_prog_errs = `{"broken.em": 1}`
	if prog_load_errors.String() != expected_prog_errs {
		t.Errorf("Prog errors not same\n\texpected: %s\n\treceived: %s", expected_prog_errs, prog_load_errors.String())
	}

}
