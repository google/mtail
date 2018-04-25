// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"errors"
	"expvar"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/go-cmp/cmp"
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
	o := Options{
		LogPathPatterns: logPathnames,
		Progs:           progPathname,
	}
	m, err := New(o)
	if err != nil {
		t.Fatalf("couldn't create mtail: %s", err)
	}

	if progPathname != "" {
		if lerr := m.l.LoadAllPrograms(); lerr != nil {
			t.Errorf("Couldn't LoadProgs: %s", lerr)
		}
	} else {
		if pErr := m.l.CompileAndRun("test", strings.NewReader(testProgram)); pErr != nil {
			t.Errorf("Couldn't compile program: %s", pErr)
		}
	}

	vm.LineCount.Set(0)
	tailer.LogCount.Set(0)
	tailer.LogRotations.Init()

	if err := m.StartTailing(); err != nil {
		t.Errorf("StartTailing failed: %s", err)
	}
	return m
}

func doOrTimeout(do func() (bool, error), deadline, interval time.Duration) (bool, error) {
	for {
		select {
		case <-time.After(deadline):
			return false, errors.New("timeout")
		case <-time.Tick(interval):
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
	// Create a logger
	hup := make(chan bool, 1)
	pathnames := []string{logFilepath}
	m := startMtailServer(t, pathnames, "")
	defer func() {
		if cerr := m.Close(); cerr != nil {
			t.Fatal(cerr)
		}
	}()

	// Rotate the log file after 500ms.
	go func() {
		<-time.After(5 * 100 * time.Millisecond)
		err = os.Rename(logFilepath, logFilepath+".1")
		if err != nil {
			t.Errorf("could not rename log file: %s", err)
		}
		// "sighup" the "logging process"
		hup <- true
	}()
	i := 0
Loop:
	for {
		select {
		case <-hup:
			// Close to flush contents
			if err = logFile.Close(); err != nil {
				t.Fatal(err)
			}
			// Received a HUP so reopen the logfile.
			logFile, err = os.OpenFile(logFilepath, os.O_RDWR|os.O_CREATE, 0)
			if err != nil {
				t.Errorf("could not create rotated log file: %s", err)
			}
			err = logFile.Chmod(0666)
			if err != nil {
				t.Errorf("could not chmod log file to read: %s", err)
			}
		case <-time.Tick(100 * time.Millisecond):
			if i >= 10 {
				break Loop
			}
			if _, werr := logFile.WriteString(fmt.Sprintf("%d\n", i)); werr != nil {
				t.Fatal(werr)
			}
			i++
		}
	}
	// Close to flush contents.
	if err = logFile.Close(); err != nil {
		t.Fatal(err)
	}
	if err = m.Close(); err != nil {
		t.Fatal(err)
	}
	expected := "10"
	if diff := cmp.Diff(expected, expvar.Get("line_count").String()); diff != "" {
		t.Errorf("line_count metric didn't match\n%s", diff)
	}
	diff := cmp.Diff("1", expvar.Get("log_rotations_total").(*expvar.Map).Get(logFilepath).String())
	if diff != "" {
		t.Errorf("log_rotations_total metric didn't match\n%s", diff)
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
	time.Sleep(10 * time.Millisecond)

	// touch log file
	logFile, err := os.Create(logFilepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer logFile.Close()
	expected := "1"
	check := func() (bool, error) {
		if tailer.LogCount.String() != expected {
			return false, nil
		}
		return true, nil
	}
	ok, err := doOrTimeout(check, 100*time.Millisecond, 10*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("Log count not increased\n\texpected: %s\n\treceived: %s", expected, tailer.LogCount.String())
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
	if tailer.LogCount.String() != expected {
		t.Errorf("Log count not increased\n\texpected: %s\n\treceived: %s", expected, tailer.LogCount.String())
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
		if tailer.LogCount.String() != "1" {
			return false, nil
		}
		if tailer.LogRotations.Get(logFilepath) != nil {
			return false, nil
		}
		return true, nil
	}
	ok, err := doOrTimeout(check3, 1*time.Second, 10*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("log count: received %s, expected 1", tailer.LogCount.String())
		t.Errorf("log rotatins: received %s, expected 0", tailer.LogRotations.String())
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
		if tailer.LogCount.String() != "1" {
			return false, nil
		}
		if tailer.LogRotations.Get(logFilepath) == nil {
			return false, nil
		}
		if tailer.LogRotations.Get(logFilepath).String() != "1" {
			return false, nil
		}
		return true, nil
	}
	ok, err = doOrTimeout(check6, 100*time.Millisecond, 10*time.Millisecond)
	if err != nil {
		buf := make([]byte, 1<<16)
		count := runtime.Stack(buf, true)
		t.Log("Timed out: Dumping goroutine stack")
		t.Log(string(buf[:count]))
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("log count: received %s, expected 1", tailer.LogCount.String())
		t.Errorf("log rotatins: received %s, expected 0", tailer.LogRotations.String())
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
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

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
			count++
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
		t.Errorf("Log count not matching\n\texpected: %d\n\t: received: %s", count, tailer.LogCount.String())
	}
}

func TestGlobAfterStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

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
	m := startMtailServer(t, []string{path.Join(workdir, "log*")}, "")
	defer m.Close()
	glog.Infof("Pausing for mtail startup.")
	time.Sleep(100 * time.Millisecond)
	count := 0
	for _, tt := range globTests {
		log, err := os.Create(tt.name)
		if err != nil {
			t.Errorf("could not create log file: %s", err)
			continue
		}
		defer log.Close()
		if tt.expected {
			count++
		}
		log.WriteString("\n")
		log.Sync()
	}
	glog.Infof("count is %d", count)
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
		t.Errorf("Log count not matching\n\texpected: %d\n\t: received: %s", count, tailer.LogCount.String())
	}
}

func TestHandleLogDeletes(t *testing.T) {
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

	if err = os.Remove(logFilepath); err != nil {
		t.Fatal(err)
	}

	expected := "0"
	check := func() (bool, error) {
		if tailer.LogCount.String() != expected {
			return false, nil
		}
		return true, nil
	}
	ok, err := doOrTimeout(check, 100*time.Millisecond, 10*time.Millisecond)
	if err != nil {
		buf := make([]byte, 1<<16)
		count := runtime.Stack(buf, true)
		t.Log("Timed out: Dumping goroutine stack")
		t.Log(string(buf[:count]))
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("Log count not decreased\n\texpected: %s\n\treceived %s", expected, tailer.LogCount.String())
	}
}

func TestHandleLogTruncate(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)

	logFilepath := path.Join(workdir, "log")
	logFile, err := os.Create(logFilepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer logFile.Close()
	pathnames := []string{logFilepath}
	m := startMtailServer(t, pathnames, "")
	defer func() {
		if cerr := m.Close(); cerr != nil {
			t.Fatal(cerr)
		}
	}()

	logFile.WriteString("x\n")
	glog.Info("Write")
	check := func() (bool, error) {
		if expvar.Get("line_count").String() != "1" {
			return false, nil
		}
		return true, nil
	}
	ok, err := doOrTimeout(check, 10*time.Second, 10*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("log line count received %s, expected 1", tailer.LogCount.String())
	}
	logFile.Truncate(0)
	glog.Infof("Truncate")
	logFile.WriteString("x\n")
	glog.Info("Write")
	check2 := func() (bool, error) {
		if expvar.Get("line_count").String() != "2" {
			return false, nil
		}
		return true, nil
	}
	ok, err = doOrTimeout(check2, 10*time.Second, 10*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	if !ok {
		t.Errorf("log line count received %s, expected 2", tailer.LogCount.String())
	}
}

func TestHandleRelativeLogAppend(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	workdir := makeTempDir(t)
	defer removeTempDir(t, workdir)

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	glog.Infof("cwd is %q", cwd)

	if err := os.Chdir(workdir); err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Error(err)
		}
	}()

	// touch log file
	logFilepath := path.Join(workdir, "log")
	logFile, err := os.Create(logFilepath)
	if err != nil {
		t.Errorf("could not touch log file: %s", err)
	}
	defer logFile.Close()
	pathnames := []string{"log"}
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
