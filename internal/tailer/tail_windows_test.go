// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build windows
// +build windows

package tailer

import (
	"testing"

	"github.com/google/mtail/internal/testutil"
)

func TestWindowsPath(t *testing.T) {
	ta, _, _, _, stop := makeTestTail(t)

	testutil.FatalIfErr(t, ta.AddPattern("C:\\somefile"))

	if _, ok := ta.globPatterns["C:\\somefile"]; !ok {
		t.Errorf("path not found in files map: %+#v", ta.globPatterns)
	}

	stop()
}
