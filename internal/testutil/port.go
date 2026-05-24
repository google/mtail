// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
package testutil

import (
	"net"
	"testing"
)

func FreePort(tb testing.TB) int {
	tb.Helper()
	// Try IPv6 first, fall back to IPv4 on environments without IPv6 (e.g. BuildBuddy RBE).
	addr, err := net.ResolveTCPAddr("tcp", "[::]:0")
	if err != nil {
		tb.Fatal(err)
	}
	l, err := net.ListenTCP("tcp", addr)
	if err != nil {
		// Fall back to IPv4.
		addr4, err4 := net.ResolveTCPAddr("tcp", "0.0.0.0:0")
		if err4 != nil {
			tb.Fatal(err4)
		}
		l, err = net.ListenTCP("tcp", addr4)
		if err != nil {
			tb.Fatal(err)
		}
	}
	defer l.Close()
	return l.Addr().(*net.TCPAddr).Port
}
