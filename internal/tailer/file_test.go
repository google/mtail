// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"context"
	"io"
	"net"
	"os"
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
	f, err := NewFile(logfile, logfile, llp, false)
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
	testutil.ExpectNoDiff(t, expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}

func TestOpenRetries(t *testing.T) {
	// Can't force a permission denied error if run as root.
	testutil.SkipIfRoot(t)

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logfile := filepath.Join(tmpDir, "log")
	if _, err := os.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0); err != nil {
		t.Fatal(err)
	}

	if _, err := NewFile(logfile, logfile, nil, false); err == nil || !os.IsPermission(err) {
		t.Fatalf("Expected a permission denied error here: %s", err)
	}
}

func TestOpenPipe(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode, has delays")
	}
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
	f, err := NewFile(logpipe, logpipe, llp, false)
	testutil.FatalIfErr(t, err)
	err = f.Read(context.Background())
	if err != io.EOF {
		testutil.FatalIfErr(t, err)
	}
}

func TestOpenSocket(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	llp := NewStubProcessor()

	logsock := filepath.Join(tmpDir, "sock")

	f, err := NewSocket(logsock, logsock, llp)
	testutil.FatalIfErr(t, err)

	l, err := net.DialUnix("unixgram", nil, &net.UnixAddr{logsock, "unixgram"})
	testutil.FatalIfErr(t, err)

	_, err = l.Write([]byte("adf\n"))
	testutil.FatalIfErr(t, err)
	llp.Add(1)

	err = f.Read(context.Background())
	testutil.FatalIfErr(t, err)
	llp.Wait()
	if f.partial.String() != "" {
		t.Errorf("partial line not empty: %q", f.partial)
	}
	expected := []*logline.LogLine{
		{context.TODO(), logsock, "adf"},
	}
	testutil.ExpectNoDiff(t, expected, llp.result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
}
