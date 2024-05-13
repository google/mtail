// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/mtail/golden"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

const exampleTimeout = 10 * time.Second

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
	{
		"examples/ntpd.mtail",
		"testdata/ntp4",
		"testdata/ntp4.golden",
	},
	{
		"examples/ntpd_peerstats.mtail",
		"testdata/xntp3_peerstats",
		"testdata/xntp3_peerstats.golden",
	},
	{
		"examples/apache_combined.mtail",
		"testdata/apache-combined.log",
		"testdata/apache-combined.golden",
	},
	{
		"examples/apache_common.mtail",
		"testdata/apache-common.log",
		"testdata/apache-common.golden",
	},
	{
		"examples/vsftpd.mtail",
		"testdata/vsftpd_log",
		"testdata/vsftpd_log.golden",
	},
	{
		"examples/vsftpd.mtail",
		"testdata/vsftpd_xferlog",
		"testdata/vsftpd_xferlog.golden",
	},
	{
		"examples/lighttpd.mtail",
		"testdata/lighttpd_access.log",
		"testdata/lighttpd_accesslog.golden",
	},
	{
		"examples/mysql_slowqueries.mtail",
		"testdata/mysql_slowqueries.log",
		"testdata/mysql_slowqueries.golden",
	},
}

func TestExamplePrograms(t *testing.T) {
	testutil.SkipIfShort(t)
	for _, tc := range exampleProgramTests {
		tc := tc
		t.Run(fmt.Sprintf("%s on %s", tc.programfile, tc.logfile),
			testutil.TimeoutTest(exampleTimeout, func(t *testing.T) { //nolint:thelper
				ctx, cancel := context.WithCancel(context.Background())
				waker, _ := waker.NewTest(ctx, 0, "waker") // oneshot means we should never need to wake the stream
				store := metrics.NewStore()
				programFile := filepath.Join("../..", tc.programfile)
				mtail, err := mtail.New(ctx, store, mtail.ProgramPath(programFile), mtail.LogPathPatterns(tc.logfile), mtail.OneShot, mtail.OmitMetricSource, mtail.DumpAstTypes, mtail.DumpBytecode, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
				testutil.FatalIfErr(t, err)

				var wg sync.WaitGroup
				wg.Add(1)
				go func() {
					defer wg.Done()
					testutil.FatalIfErr(t, mtail.Run())
				}()
				// Oneshot mode means we can wait for shutdown before cancelling.
				wg.Wait()
				cancel()

				g, err := os.Open(tc.goldenfile)
				testutil.FatalIfErr(t, err)
				defer g.Close()

				goldenStore := golden.ReadTestData(g, tc.programfile)

				var storeList metrics.MetricSlice
				store.Range(func(m *metrics.Metric) error {
					storeList = append(storeList, m)
					return nil
				})

				testutil.ExpectNoDiff(t, goldenStore, storeList, testutil.SortSlices(metrics.Less), testutil.IgnoreUnexported(metrics.Metric{}, sync.RWMutex{}, datum.String{}))
			}))
	}
}

// This test only compiles examples, but has coverage over all examples
// provided.  This ensures we ship at least syntactically correct examples.
func TestCompileExamplePrograms(t *testing.T) {
	testutil.SkipIfShort(t)
	matches, err := filepath.Glob("../../examples/*.mtail")
	testutil.FatalIfErr(t, err)
	for _, tc := range matches {
		tc := tc
		name := filepath.Base(tc)
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			s := metrics.NewStore()
			mtail, err := mtail.New(ctx, s, mtail.ProgramPath(tc), mtail.CompileOnly, mtail.OmitMetricSource, mtail.DumpAstTypes, mtail.DumpBytecode)
			testutil.FatalIfErr(t, err)
			// Ensure that run shuts down for CompileOnly
			testutil.FatalIfErr(t, mtail.Run())
			cancel()
		})
	}
}

func BenchmarkProgram(b *testing.B) {
	for _, bm := range exampleProgramTests {
		bm := bm
		b.Run(fmt.Sprintf("%s on %s", bm.programfile, bm.logfile), func(b *testing.B) {
			b.ReportAllocs()
			logDir := testutil.TestTempDir(b)
			logFile := filepath.Join(logDir, "test.log")
			log := testutil.TestOpenFile(b, logFile)
			ctx, cancel := context.WithCancel(context.Background())
			waker, awaken := waker.NewTest(ctx, 1, "streams")
			store := metrics.NewStore()
			programFile := filepath.Join("../..", bm.programfile)
			mtail, err := mtail.New(ctx, store, mtail.ProgramPath(programFile), mtail.LogPathPatterns(log.Name()), mtail.LogstreamPollWaker(waker))
			testutil.FatalIfErr(b, err)

			var wg sync.WaitGroup
			wg.Add(1)
			go func() {
				defer wg.Done()
				testutil.FatalIfErr(b, mtail.Run())
			}()

			var total int64
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				l, err := os.Open(bm.logfile)
				testutil.FatalIfErr(b, err)
				count, err := io.Copy(log, l)
				testutil.FatalIfErr(b, err)
				total += count
				awaken(1, 1)
			}
			cancel()
			wg.Wait()
			b.StopTimer()
			b.SetBytes(total)
		})
	}
}
