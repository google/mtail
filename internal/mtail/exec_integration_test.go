// Copyright 2024 Google Inc.  All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"context"
	"errors"
	"fmt"
	"os/exec"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/testutil"
)

var mtailPath string

func init() {
	path, err := exec.LookPath(filepath.Join("..", "..", "mtail"))
	if errors.Is(err, exec.ErrDot) {
		err = nil
	}
	if err != nil {
		glog.Infof("exec_integration_test init(): %v", err)
	}
	mtailPath = path
}

func TestExecMtail(t *testing.T) {
	if mtailPath == "" {
		t.Log("mtail binary not found, skipping")
		t.Skip()
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
