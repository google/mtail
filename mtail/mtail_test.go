// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
)

const testProgram = "/$/ { }\n"

func makeTempDir(t *testing.T) (workdir string) {
	var err error
	if workdir, err = ioutil.TempDir("", "mtail_test"); err != nil {
		t.Fatalf("ioutil.TempDir failed: %s", err)
	}
	return
}

func removeTempDir(t *testing.T, workdir string) {
	if err := os.RemoveAll(workdir); err != nil {
		t.Fatalf("os.RemoveAll failed: %s", err)
	}
}

func startMtailServer(t *testing.T, logPathnames []string, progPathname string) *MtailServer {
	o := Options{LogPathPatterns: logPathnames}
	m, err := New(o)
	if err != nil {
		t.Fatalf("couldn't create mtail: %s", err)
	}

	if progPathname != "" {
		m.l.LoadProgs(progPathname)
	} else {
		if pErr := m.l.CompileAndRun("test", strings.NewReader(testProgram)); pErr != nil {
			t.Errorf("Couldn't compile program: %s", pErr)
		}
	}

	vm.LineCount.Set(0)

	m.StartTailing()
	return m
}

func doOrTimeout(do func() (bool, error), deadline, interval time.Duration) (bool, error) {
	timeout := time.After(deadline)
	ticker := time.Tick(interval)
	for {
		select {
		case <-timeout:
			return false, errors.New("timeout")
		case <-ticker:
			ok, err := do()
			if err != nil {
				return false, err
			} else if ok {
				return true, nil
			}
		}
	}
}

func TestHandleLogUpdates(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)
	// touch log file
	logFilepath := path.Join(workdir, "log")
	logFile, err := os.Create(logFilepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer logFile.Close()
	pathnames := []string{logFilepath}
	m := startMtailServer(t, pathnames, "")
	defer m.Close()
	inputLines := []string{"hi", "hi2", "hi3"}
	for i, x := range inputLines {
		// write to log file
		logFile.WriteString(x + "\n")
		// check log line count increase
		expected := fmt.Sprintf("%d", i+1)
		check := func() (bool, error) {
			if vm.LineCount.String() != expected {
				return false, nil
			}
			return true, nil
		}
		ok, err := doOrTimeout(check, 100*time.Millisecond, 10*time.Millisecond)
		if err != nil {
			t.Fatal(err)
		}
		if !ok {
			t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, vm.LineCount.String())
			buf := make([]byte, 1<<16)
			count := runtime.Stack(buf, true)
			fmt.Println(string(buf[:count]))
		}
	}
}

func TestHandleLogRotation(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)
	logFilepath := path.Join(workdir, "log")
	// touch log file
	logFile, err := os.Create(logFilepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer logFile.Close()
	// Create a logger
	hup := make(chan bool, 1)
	pathnames := []string{logFilepath}
	m := startMtailServer(t, pathnames, "")
	defer m.Close()

	go func() {
		for {
			select {
			case <-time.After(5 * 100 * time.Millisecond):
				err = os.Rename(logFilepath, logFilepath+".1")
				if err != nil {
					t.Errorf("could not rename log file: %s", err)
				}
				hup <- true
				return
			}
		}
	}()
	i := 0
	running := true
	for running {
		select {
		case <-hup:
			// touch log file
			logFile, err = os.OpenFile(logFilepath, os.O_RDWR|os.O_CREATE, 0)
			if err != nil {
				t.Errorf("could not create rotated log file: %s", err)
			}
			defer logFile.Close()
			time.Sleep(1 * time.Millisecond)
			err = logFile.Chmod(0666)
			if err != nil {
				t.Errorf("could not chmod log file to read: %s", err)
			}
		default:
			logFile.WriteString(fmt.Sprintf("%d\n", i))
			time.Sleep(100 * time.Millisecond)
			i++
			if i >= 10 {
				running = false
			}
		}
	}
	expected := "10"
	if vm.LineCount.String() != expected {
		t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, vm.LineCount.String())
	}
}

func TestHandleNewLogAfterStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)
	// Start up mtail
	logFilepath := path.Join(workdir, "log")
	pathnames := []string{logFilepath}
	m := startMtailServer(t, pathnames, "")
	defer m.Close()

	// touch log file
	logFile, err := os.Create(logFilepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer logFile.Close()
	inputLines := []string{"hi", "hi2", "hi3"}
	for _, x := range inputLines {
		// write to log file
		logFile.WriteString(x + "\n")
		logFile.Sync()
	}
	// check log line count increase
	expected := fmt.Sprintf("%d", len(inputLines))
	check := func() (bool, error) {
		if vm.LineCount.String() != expected {
			return false, nil
		}
		return true, nil
	}
	ok, err := doOrTimeout(check, 100*time.Millisecond, 10*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, vm.LineCount.String())
	}
}

func TestHandleNewLogIgnored(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)
	// Start mtail
	logFilepath := path.Join(workdir, "log")
	pathnames := []string{logFilepath}
	m := startMtailServer(t, pathnames, "")
	defer m.Close()

	// touch log file
	newLogFilepath := path.Join(workdir, "log1")

	logFile, err := os.Create(newLogFilepath)
	if err != nil {
		t.Errorf("could not open log file: %s", err)
	}
	defer logFile.Close()
	expected := "0"
	if vm.LineCount.String() != expected {
		t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, vm.LineCount.String())
	}
}

func TestHandleSoftLinkChange(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)

	logFilepath := path.Join(workdir, "log")
	pathnames := []string{logFilepath}
	m := startMtailServer(t, pathnames, "")
	defer m.Close()

	trueLog1, err := os.Create(logFilepath + ".true1")
	if err != nil {
		t.Errorf("could not create log file: %s", err)
	}
	defer trueLog1.Close()
	err = os.Symlink(logFilepath+".true1", logFilepath)
	if err != nil {
		t.Errorf("could not create symlink: %s", err)
	}
	inputLines := []string{"hi1", "hi2", "hi3"}
	for _, x := range inputLines {
		trueLog1.WriteString(x + "\n")
		trueLog1.Sync()
	}
	check3 := func() (bool, error) {
		if vm.LineCount.String() != "3" {
			return false, nil
		}
		return true, nil
	}
	ok, err := doOrTimeout(check3, 1*time.Second, 10*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("line count not matched: received %s, expected 3", vm.LineCount.String())
	}
	trueLog2, err := os.Create(logFilepath + ".true2")
	if err != nil {
		t.Errorf("could not create log file: %s", err)
	}
	defer trueLog2.Close()
	err = os.Remove(logFilepath)
	if err != nil {
		t.Errorf("could not delete symlink: %s", err)
	}
	err = os.Symlink(logFilepath+".true2", logFilepath)
	if err != nil {
		t.Errorf("could not create symlink: %s", err)
	}
	for _, x := range inputLines {
		trueLog2.WriteString(x + "\n")
		trueLog2.Sync()
	}
	check6 := func() (bool, error) {
		if vm.LineCount.String() != "6" {
			return false, nil
		}
		return true, nil
	}
	ok, err = doOrTimeout(check6, 100*time.Millisecond, 10*time.Millisecond)
	if err != nil {
		buf := make([]byte, 1<<16)
		count := runtime.Stack(buf, true)
		fmt.Println(string(buf[:count]))
		t.Fatal(err)

	}
	if !ok {
		t.Errorf("line count not matched: received %s, expected 6", vm.LineCount.String())
	}
	_, err = os.Stat(logFilepath + ".true1")
	if err != nil {
		t.Errorf("stat failed on %s: %s", logFilepath+".true1", err)
	}
	_, err = os.Stat(logFilepath + ".true2")
	if err != nil {
		t.Errorf("stat failed on %s: %s", logFilepath+".true2", err)
	}
}

func TestGlob(t *testing.T) {
	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)

	globTests := []struct {
		name     string
		expected bool
	}{
		{
			path.Join(workdir, "log1"),
			true,
		},
		{
			path.Join(workdir, "log2"),
			true,
		},
		{
			path.Join(workdir, "1log"),
			false,
		},
	}
	count := 0
	for _, tt := range globTests {
		log, err := os.Create(tt.name)
		if err != nil {
			t.Errorf("could not create log file: %s", err)
			continue
		}
		defer log.Close()
		if tt.expected {
			count += 1
		}
		log.WriteString("\n")
		log.Sync()
	}
	m := startMtailServer(t, []string{path.Join(workdir, "log*")}, "")
	defer m.Close()
	check := func() (bool, error) {
		if tailer.LogCount.String() != fmt.Sprintf("%d", count) {
			glog.V(1).Infof("tailer is %q, count is %d", tailer.LogCount.String(), count)
			return false, nil
		}
		return true, nil
	}
	ok, err := doOrTimeout(check, 10*time.Second, 100*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("Log count not matching\n\texpecteed: %s\n\t: received: %s", count, tailer.LogCount.String())
	}
}
