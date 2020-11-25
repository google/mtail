// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
)

func TestFileStreamPoll(t *testing.T) {
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "log")
	f := testutil.TestOpenFile(t, name)
	ps := NewStubProcessor()

	ctx, cancel := context.WithCancel(context.Background())
	fs, err := logstream.New(ctx, &wg, name, ps)
	testutil.FatalIfErr(t, err)

	ps.Add(1)
	testutil.WriteString(t, f, "yo\n")
	fs.Poll()

	ps.Wait()
	expected := []*logline.LogLine{
		{context.TODO(), name, "yo"},
	}
	testutil.ExpectNoDiff(t, expected, ps.Result, testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()
}
