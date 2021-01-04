// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"path"
	"strings"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
)

func TestNewLoader(t *testing.T) {
	store := metrics.NewStore()
	lines := make(chan *logline.LogLine)
	l, err := NewLoader(lines, "", store)
	testutil.FatalIfErr(t, err)
	defer l.Close()
	close(lines)
}

func TestCompileAndRun(t *testing.T) {
	var testProgram = "/$/ {}\n"
	store := metrics.NewStore()
	lines := make(chan *logline.LogLine)
	l, err := NewLoader(lines, "", store)
	testutil.FatalIfErr(t, err)
	defer l.Close()
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
}

var testProgram = "/$/ {}\n"

var testProgFiles = []string{
	"test.wrongext",
	"test.mtail",
	".test",
}

func TestLoadProg(t *testing.T) {
	store := metrics.NewStore()
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()
	lines := make(chan *logline.LogLine)
	l, err := NewLoader(lines, tmpDir, store)
	testutil.FatalIfErr(t, err)
	defer l.Close()

	for _, name := range testProgFiles {
		f := testutil.TestOpenFile(t, path.Join(tmpDir, name))
		n, err := f.WriteString(testProgram)
		testutil.FatalIfErr(t, err)
		glog.Infof("Wrote %d bytes", n)
		err = l.LoadProgram(path.Join(tmpDir, name))
		testutil.FatalIfErr(t, err)
	}
	close(lines)
}
