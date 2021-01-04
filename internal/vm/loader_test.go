// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"context"
	"path"
	"strings"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
)

func TestNewLoader(t *testing.T) {
	store := metrics.NewStore()
	ctx, cancel := context.WithCancel(context.Background())
	l, err := NewLoader(ctx, "", store)
	defer l.Close()
	testutil.FatalIfErr(t, err)
	cancel()
}

func TestCompileAndRun(t *testing.T) {
	var testProgram = "/$/ {}\n"
	store := metrics.NewStore()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	l, err := NewLoader(ctx, "", store)
	defer l.Close()
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
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	l, err := NewLoader(ctx, tmpDir, store)
	defer l.Close()
	testutil.FatalIfErr(t, err)

	for _, name := range testProgFiles {
		f := testutil.TestOpenFile(t, path.Join(tmpDir, name))
		n, err := f.WriteString(testProgram)
		testutil.FatalIfErr(t, err)
		glog.Infof("Wrote %d bytes", n)
		err = l.LoadProgram(path.Join(tmpDir, name))
		testutil.FatalIfErr(t, err)
	}
}
