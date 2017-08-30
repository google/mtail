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
	"github.com/spf13/afero"
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
	{
		"examples/filename.mtail",
		"testdata/else.log",
		"testdata/filename.golden",
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
			fs := &afero.OsFs{}
			logs := []string{tc.logfile}
			o := mtail.Options{Progs: tc.programfile, LogPathPatterns: logs, W: w, FS: fs, Store: store}
			o.OneShot = true
			o.OmitMetricSource = true
			o.DumpAstTypes = true
			o.DumpBytecode = true
			mtail, err := mtail.New(o)
			if err != nil {
				t.Fatalf("create mtail failed: %s", err)
			}

			mtail.Serve()

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
				t.Error(diff)
				t.Logf(" Golden metrics: %s", golden_store.Metrics)
				t.Logf("Program metrics: %s", store.Metrics)
			}
		})
	}
}
