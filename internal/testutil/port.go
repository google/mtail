// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
package testutil

import (
	"net"
	"testing"
)

func FreePort(tb testing.TB) int {
	tb.Helper()
	addr, err := net.ResolveTCPAddr("tcp", "[::]:0")
	if err != nil {
		tb.Fatal(err)
	}
	l, err := net.ListenTCP("tcp", addr)
	if err != nil {
		tb.Fatal(err)
	}
	defer l.Close()
	return l.Addr().(*net.TCPAddr).Port
}
