// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"expvar"
	"fmt"
	"os"
	"path"
	"runtime"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

const testProgram = "/$/ { }\n"

func startMtailServer(t *testing.T, options ...func(*Server) error) *Server {
	expvar.Get("lines_total").(*expvar.Int).Set(0)
	expvar.Get("log_count").(*expvar.Int).Set(0)
	expvar.Get("log_rotations_total").(*expvar.Map).Init()
	expvar.Get("prog_loads_total").(*expvar.Map).Init()

	w, err := watcher.NewLogWatcher(0, true)
	if err != nil {
		t.Errorf("Couodn't make a log watcher: %s", err)
	}
	m, err := New(metrics.NewStore(), w, options...)
	testutil.FatalIfErr(t, err)
	if pErr := m.l.CompileAndRun("test", strings.NewReader(testProgram)); pErr != nil {
		t.Errorf("Couldn't compile program: %s", pErr)
	}

	if err := m.StartTailing(); err != nil {
		t.Errorf("StartTailing failed: %s", err)
	}
	return m
}

// func TestHandleLogDeletes(t *testing.T) {
// 	if testing.Short() {
// 		t.Skip("skipping test in short mode")
// 	}
//	workdir, rmWorkdir := testutil.TestTempDir(t)
//	defer rmWorkdir()
// 	// touch log file
// 	logFilepath := path.Join(workdir, "log")
// 	logFile, err := os.Create(logFilepath)
// 	if err != nil {
// 		t.Errorf("could not touch log file: %s", err)
// 	}
// 	defer logFile.Close()
// 	m := startMtailServer(t, LogPathPatterns(logFilepath))
// 	defer m.Close(true)

// 	if err = os.Remove(logFilepath); err != nil {
// 		t.Fatal(err)
// 	}

// 	expected := "0"
// 	check := func() (bool, error) {
// 		if expvar.Get("log_count").String() != expected {
// 			return false, nil
// 		}
// 		return true, nil
// 	}
// 	ok, err := testutil.DoOrTimeout(check, 100*time.Millisecond, 10*time.Millisecond)
// 	if err != nil {
// 		buf := make([]byte, 1<<16)
// 		count := runtime.Stack(buf, true)
// 		t.Log("Timed out: Dumping goroutine stack")
// 		t.Log(string(buf[:count]))
// 		t.Fatal(err)
// 	}
// 	if !ok {
// 		t.Errorf("Log count not decreased\n\texpected: %s\n\treceived %s", expected, expvar.Get("log_count").String())
// 	}
// }

func TestHandleRelativeLogAppend(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	cwd, err := os.Getwd()
	testutil.FatalIfErr(t, err)
	glog.Infof("cwd is %q", cwd)

	if cerr := os.Chdir(workdir); cerr != nil {
		t.Fatal(cerr)
	}
	defer func() {
		if cerr := os.Chdir(cwd); err != nil {
			t.Error(cerr)
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
	m := startMtailServer(t, LogPathPatterns(pathnames...))
	defer m.Close(true)
	inputLines := []string{"hi", "hi2", "hi3"}
	for i, x := range inputLines {
		// write to log file
		testutil.WriteString(t, logFile, x+"\n")
		// check log line count increase
		expected := fmt.Sprintf("%d", i+1)
		check := func() (bool, error) {
			if expvar.Get("lines_total").String() != expected {
				return false, nil
			}
			return true, nil
		}
		ok, err := testutil.DoOrTimeout(check, 100*time.Millisecond, 10*time.Millisecond)
		testutil.FatalIfErr(t, err)
		if !ok {
			t.Errorf("Line count not increased\n\texpected: %s\n\treceived: %s", expected, expvar.Get("lines_total").String())
			buf := make([]byte, 1<<16)
			count := runtime.Stack(buf, true)
			fmt.Println(string(buf[:count]))
		}
	}

}

func TestProgramReloadNoDuplicateMetrics(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in shor tmode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	logDir := path.Join(workdir, "logs")
	if err := os.Mkdir(logDir, 0777); err != nil {
		t.Fatal(err)
	}
	progDir := path.Join(workdir, "progs")
	if err := os.Mkdir(progDir, 0777); err != nil {
		t.Fatal(err)
	}

	logFilepath := path.Join(logDir, "log")
	logFile, err := os.Create(logFilepath)
	testutil.FatalIfErr(t, err)
	defer logFile.Close()

	m := startMtailServer(t, ProgramPath(progDir), LogPathPatterns(logDir+"/*"))
	defer m.Close(true)
	store := m.store

	v := expvar.Get("prog_loads_total").(*expvar.Map).Get("program.mtail")
	if v != nil {
		t.Log(v)
	}

	progpath := path.Join(progDir, "program.mtail")
	p, err := os.Create(progpath)
	testutil.FatalIfErr(t, err)
	testutil.WriteString(t, p, "counter foo\n/^foo$/ {\n foo++\n }\n")
	testutil.FatalIfErr(t, p.Close())

	check := func() (bool, error) {
		v := expvar.Get("prog_loads_total").(*expvar.Map).Get("program.mtail")
		if v == nil {
			return false, nil
		}
		n, nerr := strconv.Atoi(v.String())
		if nerr != nil {
			return false, nerr
		}
		if n < 1 {
			return false, nil
		}
		return true, nil
	}
	ok, err := testutil.DoOrTimeout(check, time.Second, 10*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if !ok {
		t.Fatal("program loads didn't increase")
	}
	mfoo := store.Metrics["foo"]
	if len(mfoo) != 1 || len(mfoo[0].LabelValues) != 1 {
		t.Errorf("Unexpected metrics content: expected a single metric with no labels, but got all this %v", mfoo)
	}

	n, err := logFile.WriteString("foo\n")
	testutil.FatalIfErr(t, err)
	if n < 4 {
		t.Fatalf("only wrote %d", n)
	}
	time.Sleep(100 * time.Millisecond)

	checkFoo := func() (bool, error) {
		store.RLock()
		v := store.Metrics["foo"][0]
		store.RUnlock()
		d, derr := v.GetDatum()
		if derr != nil {
			return false, derr
		}
		if d.ValueString() != "1" {
			t.Log(d)
			return false, nil
		}
		return true, nil
	}
	ok, err = testutil.DoOrTimeout(checkFoo, time.Second, 10*time.Millisecond)
	if err != nil {
		t.Error(err)
	}
	if !ok {
		t.Fatal("foo didn't increase")
	}

	p, err = os.Create(progpath)
	testutil.FatalIfErr(t, err)
	testutil.WriteString(t, p, "counter foo\n/^foo$/ {\n foo++\n }\n")
	testutil.FatalIfErr(t, p.Close())

	check2 := func() (bool, error) {
		v := expvar.Get("prog_loads_total")
		v = v.(*expvar.Map).Get("program.mtail")
		if v == nil {
			return false, nil
		}
		n, nerr := strconv.Atoi(v.String())
		if nerr != nil {
			return false, nerr
		}
		if n < 2 {
			return false, nil
		}
		return true, nil
	}
	ok, err = testutil.DoOrTimeout(check2, time.Second, 10*time.Millisecond)
	if err != nil {
		t.Error(err)
	}
	if !ok {
		t.Error("program loads didn't increase")
	}
	store.Lock()
	mfoo = store.Metrics["foo"]
	if len(mfoo) != 1 || len(mfoo[0].LabelValues) != 1 {
		t.Errorf("Unexpected metrics content: expected a single metric with no labels, but got all this: %v", mfoo)
	}
	store.Unlock()
}

func TestBuildInfo(t *testing.T) {
	buildInfo := BuildInfo{
		Branch:   "foo",
		Version:  "bar",
		Revision: "baz",
	}

	buildInfoWant := fmt.Sprintf(
		"mtail version bar git revision baz go version %s go arch %s go os %s",
		runtime.Version(),
		runtime.GOARCH,
		runtime.GOOS,
	)
	buildInfoGot := buildInfo.String()

	if buildInfoWant != buildInfoGot {
		t.Errorf("Unexpected build info string, want: %q, got: %q", buildInfoWant, buildInfoGot)
	}
}

func TestFilenameRegexIgnore(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	globTests := []struct {
		name     string
		expected bool
	}{
		{
			path.Join(workdir, "log1"),
			true,
		},
		{
			path.Join(workdir, "log1.gz"),
			false,
		},
		{
			path.Join(workdir, "log2gz"),
			true,
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
		testutil.WriteString(t, log, "\n")
		testutil.FatalIfErr(t, err)
		testutil.FatalIfErr(t, log.Sync())
	}
	m := startMtailServer(t, LogPathPatterns(path.Join(workdir, "log*")), IgnoreRegexPattern("\\.gz"))
	defer m.Close(true)
	check := func() (bool, error) {
		if expvar.Get("log_count").String() != fmt.Sprintf("%d", count) {
			glog.V(1).Infof("tailer is %q, count is %d", expvar.Get("log_count").String(), count)
			return false, nil
		}
		return true, nil
	}
	ok, err := testutil.DoOrTimeout(check, 10*time.Second, 100*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if !ok {
		t.Errorf("Log count not matching\n\texpected: %d\n\t: received: %s", count, expvar.Get("log_count").String())
	}
}

func TestIgnoreFolder(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	globTests := []struct {
		name     string
		isFolder bool
		expected bool
	}{
		{
			path.Join(workdir, "log1"),
			false,
			true,
		},
		{
			path.Join(workdir, "logarchive"),
			true,
			false,
		},
		{
			path.Join(workdir, "log2.gz"),
			false,
			false,
		},
	}
	count := 0
	for _, tt := range globTests {
		var err error
		var log *os.File

		if tt.isFolder {
			err = os.Mkdir(tt.name, 0700)
			testutil.FatalIfErr(t, err)
			continue
		} else {
			log, err = os.Create(tt.name)
		}

		if !tt.isFolder && tt.expected {
			count++
		}
		defer log.Close()
		testutil.FatalIfErr(t, err)
		testutil.WriteString(t, log, "\n")
		testutil.FatalIfErr(t, log.Sync())
	}
	m := startMtailServer(t, LogPathPatterns(path.Join(workdir, "log*")), IgnoreRegexPattern("\\.gz"))
	defer m.Close(true)
	check := func() (bool, error) {
		if expvar.Get("log_count").String() != fmt.Sprintf("%d", count) {
			glog.V(1).Infof("tailer is %q, count is %d", expvar.Get("log_count").String(), count)
			return false, nil
		}
		return true, nil
	}
	ok, err := testutil.DoOrTimeout(check, 10*time.Second, 100*time.Millisecond)
	testutil.FatalIfErr(t, err)
	if !ok {
		t.Errorf("Log count not matching\n\texpected: %d\n\t: received: %s", count, expvar.Get("log_count").String())
	}
}
