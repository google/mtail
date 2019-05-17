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
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/testutil"
)

func TestReadPartial(t *testing.T) {
	lines := make(chan *logline.LogLine, 1)

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	logfile := path.Join(tmpDir, "t")

	fd := testutil.TestOpenFile(t, logfile)
	f, err := NewFile(logfile, lines, false)
	if err != nil {
		t.Fatal(err)
	}

	done := make(chan struct{})
	wg := sync.WaitGroup{}
	wg.Add(1)
	var result []*logline.LogLine
	go func() {
		for line := range lines {
			result = append(result, line)
			wg.Done()
		}
		close(done)
	}()
	err = f.Read(context.Background())
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	if f.partial.String() != "" {
		t.Errorf("partial line not empty: %q", f.partial)
	}
	testutil.WriteString(t, fd, "o")
	testutil.WriteString(t, fd, "hi")
	// memmapfs shares data structure here and in code under test so reset the file offset
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
	_, err = fd.Seek(-1, io.SeekEnd)
	testutil.FatalIfErr(t, err)
	err = f.Read(context.Background())
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	// sync with reader goroutine
	close(lines)
	<-done
	if f.partial.String() != "" {
		t.Errorf("partial line not empty: %q", f.partial)
	}
	expected := []*logline.LogLine{
		{context.TODO(), logfile, "ohi"},
	}
	diff := testutil.Diff(expected, result, testutil.IgnoreFields(logline.LogLine{}, "Context"))
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
