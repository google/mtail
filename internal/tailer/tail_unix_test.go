// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix
// +build unix

package tailer

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/testutil"
)

// TestTailerOpenRetries is a unix-specific test because on Windows, it is not possible to create a file
// that you yourself cannot read (minimum permissions are 0222).
func TestTailerOpenRetries(t *testing.T) {
	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	ta, lines, awaken, dir, stop := makeTestTail(t)

	logfile := filepath.Join(dir, "log")
	if _, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0); err != nil {
		t.Fatal(err)
	}

	testutil.FatalIfErr(t, ta.AddPattern(logfile))

	if err := ta.TailPath(logfile); err == nil || !os.IsPermission(err) {
		t.Fatalf("Expected a permission denied error here: %s", err)
	}
	testutil.FatalIfErr(t, ta.PollLogPatterns())
	testutil.FatalIfErr(t, ta.PollLogStreamsForCompletion())
	glog.Info("remove")
	if err := os.Remove(logfile); err != nil {
		t.Fatal(err)
	}
	testutil.FatalIfErr(t, ta.PollLogPatterns())
	testutil.FatalIfErr(t, ta.PollLogStreamsForCompletion())
	glog.Info("openfile")
	f, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0)
	defer f.Close()
	testutil.FatalIfErr(t, err)
	testutil.FatalIfErr(t, ta.PollLogPatterns())
	testutil.FatalIfErr(t, ta.PollLogStreamsForCompletion())
	glog.Info("chmod")
	if err := os.Chmod(logfile, 0o666); err != nil {
		t.Fatal(err)
	}
	testutil.FatalIfErr(t, ta.PollLogPatterns())
	testutil.FatalIfErr(t, ta.PollLogStreamsForCompletion())
	awaken(1) // force sync to EOF
	glog.Info("write string")
	testutil.WriteString(t, f, "\n")
	awaken(1)

	stop()

	received := testutil.LinesReceived(lines)
	expected := []*logline.LogLine{
		{context.Background(), logfile, ""},
	}
	testutil.ExpectNoDiff(t, expected, received, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}
