// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/user"
	"path"
	"path/filepath"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/testutil"
	"golang.org/x/sys/unix"
)

func TestReadPartial(t *testing.T) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logfile := path.Join(tmpDir, "t")

	llp := NewStubProcessor()

	fd := testutil.TestOpenFile(t, logfile)
	f, err := NewFile(logfile, llp, false)
	testutil.FatalIfErr(t, err)

	err = f.Read(context.Background())
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	if f.partial.String() != "" {
		t.Errorf("partial line not empty: %q", f.partial)
	}
	testutil.WriteString(t, fd, "o")
	testutil.WriteString(t, fd, "hi")

	_, err = fd.Seek(0, 0)
	testutil.FatalIfErr(t, err)
	err = f.Read(context.Background())
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	if f.partial.String() != "ohi" {
		t.Errorf("partial line not expected: %q", f.partial)
	}
	// reset the cursor again
	_, err = fd.Seek(3, io.SeekStart)
	testutil.FatalIfErr(t, err)
	testutil.WriteString(t, fd, "\n")
	llp.Add(1)

	_, err = fd.Seek(-1, io.SeekEnd)
	testutil.FatalIfErr(t, err)
	err = f.Read(context.Background())
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	// sync with reader goroutine
	if f.partial.String() != "" {
		t.Errorf("partial line not empty: %q", f.partial)
	}
	llp.Wait()
	expected := []*logline.LogLine{
		{context.TODO(), logfile, "ohi"},
	}
	diff := testutil.Diff(expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
	if diff != "" {
		t.Errorf("result didn't match:\n%s", diff)
	}
}

func TestOpenRetries(t *testing.T) {
	// Can't force a permission denied error if run as root.
	u, err := user.Current()
	if err != nil {
		t.Skip(fmt.Sprintf("Couldn't determine current user id: %s", err))
	}
	if u.Uid == "0" {
		t.Skip("Skipping test when run as root")
	}

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logfile := filepath.Join(tmpDir, "log")
	if _, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0); err != nil {
		t.Fatal(err)
	}

	if _, err := NewFile(logfile, nil, false); err == nil || !os.IsPermission(err) {
		t.Fatalf("Expected a permission denied error here: %s", err)
	}
}

func TestOpenPipe(t *testing.T) {
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	llp := NewStubProcessor()

	logpipe := filepath.Join(tmpDir, "fifo")
	err := unix.Mkfifo(logpipe, 0666)
	testutil.FatalIfErr(t, err)
	_, err = os.Stat(logpipe)
	testutil.FatalIfErr(t, err)

	p, err := os.OpenFile(logpipe, os.O_RDWR, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)

	p.WriteString("1\n")
	llp.Add(1)
	f, err := NewFile(logpipe, llp, false)
	testutil.FatalIfErr(t, err)
	err = f.Read(context.Background())
	if err != io.EOF {
		testutil.FatalIfErr(t, err)
	}
}
