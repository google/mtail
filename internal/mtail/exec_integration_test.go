// Copyright 2024 Google Inc.  ll Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"context"
	"errors"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	"github.com/golang/glog"
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
		"-one_shot",
		"-one_shot_format=prometheus",
	}
	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, mtailPath, cs...)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Logf("%s", out)
		t.Error(err)
	}
}
