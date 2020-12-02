// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"net"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
)

func TestSocketStreamRead(t *testing.T) {
	t.Skip("logstream.New cannot stat a nonexistent socket")
	var wg sync.WaitGroup

	tmpDir, rmTmpDir := testutil.TestTempDir(t)
	defer rmTmpDir()

	name := filepath.Join(tmpDir, "sock")

	sp := NewStubProcessor()

	ctx, cancel := context.WithCancel(context.Background())
	ss, err := logstream.New(ctx, &wg, name, sp)
	testutil.FatalIfErr(t, err)
	ss.Wake() // Synchronise past socket creation

	s, err := net.DialUnix("unixgram", nil, &net.UnixAddr{name, "unixgram"})
	testutil.FatalIfErr(t, err)

	sp.ExpectLinesReceived(1)
	_, err = s.Write([]byte("1\n"))
	testutil.FatalIfErr(t, err)
	ss.Wake()

	sp.Verify()
	expected := []logline.LogLine{
		{context.TODO(), name, "1"},
	}
	testutil.ExpectNoDiff(t, expected, sp.Result(), testutil.IgnoreFields(logline.LogLine{}, "Context"))

	cancel()
	wg.Wait()
}
