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

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/mtail/golden"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
	"golang.org/x/sys/unix"
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
		"examples/otherwise.mtail",
		"testdata/otherwise.log",
		"testdata/otherwise.golden",
	},
	{
		"examples/types.mtail",
		"testdata/types.log",
		"testdata/types.golden",
	},
	// TODO(jaq): getfilename() is broken by poll log patterns in tailer constructor.
	// {
	// 	"examples/filename.mtail",
	// 	"testdata/else.log",
	// 	"testdata/filename.golden",
	// },
	{
		"examples/logical.mtail",
		"testdata/logical.log",
		"testdata/logical.golden",
	},
	{
		"examples/strcat.mtail",
		"testdata/strcat.log",
		"testdata/strcat.golden",
	},
	{
		"examples/typed-comparison.mtail",
		"testdata/typed-comparison.log",
		"testdata/typed-comparison.golden",
	},
	{
		"examples/match-expression.mtail",
		"testdata/match-expression.log",
		"testdata/match-expression.golden",
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
		"examples/metric-as-rvalue.mtail",
		"testdata/metric-as-rvalue.log",
		"testdata/metric-as-rvalue.golden",
	},
	{
		"examples/stringy.mtail",
		"testdata/stringy.log",
		"testdata/stringy.golden",
	},
	{
		"examples/ip-addr.mtail",
		"testdata/ip-addr.log",
		"testdata/ip-addr.golden",
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
		t.Run(fmt.Sprintf("%s on %s", tc.programfile, tc.logfile), func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			waker, _ := waker.NewTest(ctx, 0) // oneshot means we should never need to wake the stream
			store := metrics.NewStore()
			programFile := filepath.Join("../..", tc.programfile)
			mtail, err := mtail.New(ctx, store, mtail.ProgramPath(programFile), mtail.LogPathPatterns(tc.logfile), mtail.OneShot, mtail.OmitMetricSource, mtail.DumpAstTypes, mtail.DumpBytecode, mtail.OmitDumpMetricStore, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
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

			goldenStore := metrics.NewStore()
			golden.ReadTestData(g, tc.programfile, goldenStore)

			testutil.ExpectNoDiff(t, goldenStore, store, testutil.IgnoreUnexported(sync.RWMutex{}, sync.Mutex{}, datum.String{}))
		})
	}
}

// This test only compiles examples, but has coverage over all examples
// provided.  This ensures we ship at least syntactically correct examples.
func TestCompileExamplePrograms(t *testing.T) {
	testutil.SkipIfShort(t)
	matches, err := filepath.Glob("../../examples/*.mtail")
	testutil.FatalIfErr(t, err)
	for _, tc := range matches {
		name := filepath.Base(tc)
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			s := metrics.NewStore()
			mtail, err := mtail.New(ctx, s, mtail.ProgramPath(tc), mtail.CompileOnly, mtail.OmitMetricSource, mtail.DumpAstTypes, mtail.DumpBytecode, mtail.OmitDumpMetricStore)
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
			waker, awaken := waker.NewTest(ctx, 1)
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
				awaken(1)
			}
			cancel()
			wg.Wait()
			b.StopTimer()
			b.SetBytes(total)
		})
	}
}

// Two mtails both alike in dignity.
func TestFilePipeStreamComparison(t *testing.T) {
	testutil.SkipIfShort(t)

	for _, tc := range exampleProgramTests {
		tc := tc
		t.Run(fmt.Sprintf("%s on %s", tc.programfile, tc.logfile), func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			waker := waker.NewTestAlways()
			fileStore, pipeStore := metrics.NewStore(), metrics.NewStore()
			programFile := filepath.Join("../..", tc.programfile)

			// Set up the pipe
			tmpDir := testutil.TestTempDir(t)

			pipeName := filepath.Join(tmpDir, filepath.Base(tc.logfile))
			testutil.FatalIfErr(t, unix.Mkfifo(pipeName, 0600))

			var wg sync.WaitGroup
			wg.Add(3)
			go func() {
				defer wg.Done()
				source, err := os.OpenFile(tc.logfile, os.O_RDONLY, 0)
				testutil.FatalIfErr(t, err)
				// not NONBLOCK to wait for pipeMtail to start reading the pipe
				pipe, err := os.OpenFile(pipeName, os.O_WRONLY, os.ModeNamedPipe)
				testutil.FatalIfErr(t, err)
				n, err := io.Copy(pipe, source)
				testutil.FatalIfErr(t, err)
				glog.Infof("Copied %d bytes into pipe", n)
				source.Close()
				pipe.Close()
			}()

			go func() {
				defer wg.Done()
				fileMtail, err := mtail.New(ctx, fileStore, mtail.ProgramPath(programFile), mtail.LogPathPatterns(tc.logfile), mtail.OneShot, mtail.OmitMetricSource, mtail.OmitDumpMetricStore, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
				if err != nil {
					t.Error(err)
				}
				if err := fileMtail.Run(); err != nil {
					t.Error(err)
				}
			}()
			pipeMtail, err := mtail.New(ctx, pipeStore, mtail.ProgramPath(programFile), mtail.LogPathPatterns(pipeName), mtail.OneShot, mtail.OmitMetricSource, mtail.OmitDumpMetricStore, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
			testutil.FatalIfErr(t, err)
			go func() {
				defer wg.Done()
				if err := pipeMtail.Run(); err != nil {
					t.Error(err)
				}
			}()

			// Oneshot mode means we can wait for shutdown before cancelling.
			wg.Wait()
			cancel()

			// Ignore the usual field and the datum.Time field as well, as the results will be unstable otherwise.
			testutil.ExpectNoDiff(t, fileStore, pipeStore, testutil.IgnoreUnexported(sync.RWMutex{}, sync.Mutex{}, datum.String{}), testutil.IgnoreFields(datum.BaseDatum{}, "Time"))
		})
	}
}
