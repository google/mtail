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

var line_counting_program = "counter line_count\n/$/ { line_count++ }"

func TestHandleLogUpdates(t *testing.T) {
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
	// start server
	pathnames := []string{log_filepath}
	vms := make([]*vm, 0)
	prog, errors := Compile("line_count", strings.NewReader(line_counting_program))
	if len(errors) > 0 {
		t.Errorf("Couldn't compile program: %s", errors)
	}
	vms = append(vms, prog)
	lines := make(chan string)
	go RunVms(vms, lines)
	StartEmtail(lines, pathnames)
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
