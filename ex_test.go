// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"fmt"
	"os"
	"testing"

	"github.com/go-test/deep"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/mtail"
	"github.com/google/mtail/testdata"
	"github.com/google/mtail/watcher"
)

var exampleProgramTests = []struct {
	programfile string // Example program file.
	logfile     string // Sample log input.
	goldenfile  string // Expected metrics after processing.
}{
	{
		"examples/rsyncd.mtail",
		"testdata/rsyncd.log",
		"testdata/rsyncd.golden",
	},
	{
		"examples/sftp.mtail",
		"testdata/sftp_chroot.log",
		"testdata/sftp_chroot.golden",
	},
	{
		"examples/dhcpd.mtail",
		"testdata/anonymised_dhcpd_log",
		"testdata/anonymised_dhcpd_log.golden",
	},
	// {
	// 	"examples/ntpd.mtail",
	// 	"testdata/ntp4",
	// 	"testdata/ntp4.golden",
	// },
	{
		"examples/ntpd.mtail",
		"testdata/xntp3_peerstats",
		"testdata/xntp3_peerstats.golden",
	},
	{
		"examples/otherwise.mtail",
		"testdata/otherwise.log",
		"testdata/otherwise.golden",
	},
	{
		"examples/else.mtail",
		"testdata/else.log",
		"testdata/else.golden",
	},
	{
		"examples/types.mtail",
		"testdata/types.log",
		"testdata/types.golden",
	},
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	for _, tc := range exampleProgramTests {
		t.Run(fmt.Sprintf("%s on %s", tc.programfile, tc.logfile), func(t *testing.T) {
			w := watcher.NewFakeWatcher()
			store := metrics.NewStore()
			o := mtail.Options{Progs: tc.programfile, W: w, Store: store}
			o.DumpAstTypes = true
			o.DumpBytecode = true
			mtail, err := mtail.New(o)
			if err != nil {
				t.Fatalf("create mtail failed: %s", err)
			}

			if _, err := mtail.OneShot(tc.logfile, false); err != nil {
				t.Fatalf("oneshot error: %s", err)
			}

			g, err := os.Open(tc.goldenfile)
			if err != nil {
				t.Fatalf("could not open golden file: %s", err)
			}
			defer g.Close()

			golden_store := metrics.NewStore()
			testdata.ReadTestData(g, tc.programfile, golden_store)

			mtail.Close()

			diff := deep.Equal(golden_store, store)

			if diff != nil {
				t.Errorf("metrics don't match:\n%v", diff)
				t.Errorf(" Golden metrics: %s", golden_store.Metrics)
				t.Errorf("Program metrics: %s", store.Metrics)
			}
		})
	}
}
