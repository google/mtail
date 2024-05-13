// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"net"
	"path/filepath"
	"testing"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestBasicUNIXSockets(t *testing.T) {
	testutil.SkipIfShort(t)
	tmpDir := testutil.TestTempDir(t)
	sockListenAddr := filepath.Join(tmpDir, "mtail_test.sock")

	_, stopM := mtail.TestStartServer(t, 1, 1, mtail.LogPathPatterns(tmpDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"), mtail.BindUnixSocket(sockListenAddr))
	defer stopM()

	glog.Infof("check that server is listening")

	addr, err := net.ResolveUnixAddr("unix", sockListenAddr)
	testutil.FatalIfErr(t, err)
	_, err = net.DialUnix("unix", nil, addr)
	testutil.FatalIfErr(t, err)
}
