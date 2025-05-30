// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/mtail"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestBadProgramFailsCompilation(t *testing.T) {
	testutil.SkipIfShort(t)
	progDir := testutil.TestTempDir(t)

	err := os.WriteFile(filepath.Join(progDir, "bad.mtail"), []byte("asdfasdf\n"), 0o666)
	testutil.FatalIfErr(t, err)

	ctx := context.Background()
	// Compile-only fails program compilation at server start, not after it's running.
	_, err = mtail.New(ctx, metrics.NewStore(), mtail.ProgramPath(progDir), mtail.CompileOnly)
	if err == nil {
		t.Error("expected error from mtail")
	}
	if !strings.Contains(err.Error(), "compile failed") {
		t.Error("compile failed not reported")
	}
}
