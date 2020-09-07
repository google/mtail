// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"io/ioutil"
	"path"
	"strings"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestBadProgramFailsCompilation(t *testing.T) {
	t.Skip("broken, need to handle compile error correctly.")
	progDir, rmProgDir := testutil.TestTempDir(t)
	defer rmProgDir()
	logDir, rmLogDir := testutil.TestTempDir(t)
	defer rmLogDir()

	err := ioutil.WriteFile(path.Join(progDir, "bad.mtail"), []byte("asdfasdf\n"), 0666)
	if err != nil {
		t.Fatal(err)
	}

	// Compile-only fails program compilation at server start, not after it's running.
	_ = mtail.TestMakeServer(t, 0, false, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir), mtail.CompileOnly)
	if err == nil {
		t.Error("expected error from mtail")
	}
	if !strings.Contains(err.Error(), "compile failed") {
		t.Error("compile failed not reported")
	}
}
