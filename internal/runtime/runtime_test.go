// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

import (
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestNewRuntime(t *testing.T) {
	store := metrics.NewStore()
	lines := make(chan *logline.LogLine)
	var wg sync.WaitGroup
	_, err := New(lines, &wg, "", store)
	testutil.FatalIfErr(t, err)
	close(lines)
	wg.Wait()
}

func TestNewRuntimeErrors(t *testing.T) {
	store := metrics.NewStore()
	lines := make(chan *logline.LogLine)
	var wg sync.WaitGroup
	_, err := New(lines, nil, "", store)
	if err == nil {
		t.Error("New(..., nil) expecting error, got nil")
	}
	_, err = New(lines, &wg, "", nil)
	if err == nil {
		t.Error("New(..., nil) expecting error, got nil")
	}
}

func TestCompileAndRun(t *testing.T) {
	testProgram := "/$/ {}\n"
	store := metrics.NewStore()
	lines := make(chan *logline.LogLine)
	var wg sync.WaitGroup
	l, err := New(lines, &wg, "", store)
	testutil.FatalIfErr(t, err)
	if err := l.CompileAndRun("Test", strings.NewReader(testProgram)); err != nil {
		t.Errorf("CompileAndRun returned error: %s", err)
	}
	l.handleMu.Lock()
	if len(l.handles) < 1 {
		t.Errorf("no vm handles: %v", l.handles)
	}
	l.handleMu.Unlock()
	l.handleMu.Lock()
	h := l.handles["Test"]
	if h == nil {
		t.Errorf("No handle for Test: %v", l.handles)
	}
	l.handleMu.Unlock()
	close(lines)
	wg.Wait()
}

var testProgram = "/$/ {}\n"

var testProgFiles = []string{
	"test.wrongext",
	"test.mtail",
	".test",
}

func TestLoadProg(t *testing.T) {
	store := metrics.NewStore()
	tmpDir := testutil.TestTempDir(t)

	lines := make(chan *logline.LogLine)
	var wg sync.WaitGroup
	l, err := New(lines, &wg, tmpDir, store)
	testutil.FatalIfErr(t, err)

	for _, name := range testProgFiles {
		f := testutil.TestOpenFile(t, filepath.Join(tmpDir, name))
		n, err := f.WriteString(testProgram)
		testutil.FatalIfErr(t, err)
		glog.Infof("Wrote %d bytes", n)
		err = l.LoadProgram(filepath.Join(tmpDir, name))
		testutil.FatalIfErr(t, err)
		f.Close()
	}
	close(lines)
	wg.Wait()
}
