// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package file

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/user"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/mtail/logline"
	"github.com/spf13/afero"
)

func TestReadPartial(t *testing.T) {
	lines := make(chan *logline.LogLine, 1)
	fs := afero.NewMemMapFs()

	logfile := "/t"

	fd, err := fs.Create(logfile)
	if err != nil {
		t.Fatal(err)
	}
	f, err := New(fs, logfile, lines, false)
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
	err = f.Read()
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	if f.partial.String() != "" {
		t.Errorf("partial line not empty: %q", f.partial)
	}
	fd.WriteString("o")
	fd.WriteString("hi")
	// memmapfs shares data structure here and in code under test so reset the file offset
	fd.Seek(0, 0)
	err = f.Read()
	if err != io.EOF {
		t.Errorf("error returned not EOF: %v", err)
	}
	if f.partial.String() != "ohi" {
		t.Errorf("partial line not expected: %q", f.partial)
	}
	// reset the cursor again
	fd.Seek(3, io.SeekStart)
	fd.WriteString("\n")
	fd.Seek(-1, io.SeekEnd)
	err = f.Read()
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
		{logfile, "ohi"},
	}
	diff := cmp.Diff(expected, result)
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

	dir, err := ioutil.TempDir("", "file_test_openretries")
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Log(err)
		}
	}()

	// Use the real filesystem because afero doesn't implement correct
	// permissions checking on OpenFile in the memfile implementation.
	fs := afero.NewOsFs()

	logfile := filepath.Join(dir, "log")
	if _, err := fs.OpenFile(logfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0); err != nil {
		t.Fatal(err)
	}

	if _, err := New(fs, logfile, nil, false); err == nil || !os.IsPermission(err) {
		t.Fatalf("Expected a permission denied error here: %s", err)
	}
}
