// Copyright 2024 Google Inc.  All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"context"
	"fmt"
	"os/exec"
	"syscall"
	"testing"
	"time"

	"github.com/bazelbuild/rules_go/go/tools/bazel"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestExecMtail(t *testing.T) {
	mtailPath, ok := bazel.FindBinary("cmd/mtail", "mtail")
	if !ok {
		t.Fatal("`mtail` not found in runfiles")
	}
	cs := []string{
		"-progs",
		"../../examples",
		"-logs", "testdata/rsyncd.log",
		"--logtostderr",
		"--v=2",
		"-one_shot",
		"-one_shot_format=prometheus",
		"-port", fmt.Sprintf("%d", testutil.FreePort(t)),
	}
	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, mtailPath, cs...)
	cmd.Cancel = func() error {
		// Kill with abort to dump stack
		return cmd.Process.Signal(syscall.SIGABRT)
	}
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Logf("%s", out)
		t.Error(err)
	}
}
