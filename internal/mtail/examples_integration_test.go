// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/mtail/golden"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
	"golang.org/x/sys/unix"
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
				waker, _ := waker.NewTest(ctx, 0) // oneshot means we should never need to wake the stream
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
		t.Run(fmt.Sprintf("%s on %s", tc.programfile, tc.logfile),
			testutil.TimeoutTest(exampleTimeout, func(t *testing.T) { //nolint:thelper
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
					fileMtail, err := mtail.New(ctx, fileStore, mtail.ProgramPath(programFile), mtail.LogPathPatterns(tc.logfile), mtail.OneShot, mtail.OmitMetricSource, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
					if err != nil {
						t.Error(err)
					}
					if err := fileMtail.Run(); err != nil {
						t.Error(err)
					}
				}()
				pipeMtail, err := mtail.New(ctx, pipeStore, mtail.ProgramPath(programFile), mtail.LogPathPatterns(pipeName), mtail.OneShot, mtail.OmitMetricSource, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
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

				var pipeMetrics, fileMetrics metrics.MetricSlice
				pipeStore.Range(func(m *metrics.Metric) error {
					pipeMetrics = append(pipeMetrics, m)
					return nil
				})
				fileStore.Range(func(m *metrics.Metric) error {
					fileMetrics = append(fileMetrics, m)
					return nil
				})

				// Ignore the datum.Time field as well, as the results will be unstable otherwise.
				testutil.ExpectNoDiff(t, fileMetrics, pipeMetrics, testutil.SortSlices(metrics.Less), testutil.IgnoreUnexported(metrics.Metric{}, sync.RWMutex{}, datum.String{}), testutil.IgnoreFields(datum.BaseDatum{}, "Time"))
			}))
	}
}

func TestFileSocketStreamComparison(t *testing.T) {
	testutil.SkipIfShort(t)

	for _, scheme := range []string{"unixgram", "unix"} {
		scheme := scheme
		for _, tc := range exampleProgramTests {
			tc := tc
			t.Run(fmt.Sprintf("%s on %s://%s", tc.programfile, scheme, tc.logfile),
				testutil.TimeoutTest(exampleTimeout, func(t *testing.T) { //nolint:thelper
					ctx, cancel := context.WithCancel(context.Background())
					waker := waker.NewTestAlways()
					fileStore, sockStore := metrics.NewStore(), metrics.NewStore()
					programFile := filepath.Join("../..", tc.programfile)

					// Set up the socket
					tmpDir := testutil.TestTempDir(t)

					sockName := filepath.Join(tmpDir, filepath.Base(tc.logfile))

					var wg sync.WaitGroup
					wg.Add(3)
					go func() {
						defer wg.Done()
						fileMtail, err := mtail.New(ctx, fileStore, mtail.ProgramPath(programFile), mtail.LogPathPatterns(tc.logfile), mtail.OneShot, mtail.OmitMetricSource, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
						if err != nil {
							t.Error(err)
						}
						if err := fileMtail.Run(); err != nil {
							t.Error(err)
						}
					}()
					sockMtail, err := mtail.New(ctx, sockStore, mtail.ProgramPath(programFile), mtail.LogPathPatterns(scheme+"://"+sockName), mtail.OneShot, mtail.OmitMetricSource, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
					testutil.FatalIfErr(t, err)
					go func() {
						defer wg.Done()
						if err := sockMtail.Run(); err != nil {
							t.Error(err)
						}
					}()

					go func() {
						defer wg.Done()
						source, err := os.OpenFile(tc.logfile, os.O_RDONLY, 0)
						testutil.FatalIfErr(t, err)
						s, err := net.DialUnix(scheme, nil, &net.UnixAddr{sockName, scheme})
						testutil.FatalIfErr(t, err)
						n, err := io.Copy(s, source)
						testutil.FatalIfErr(t, err)
						glog.Infof("Copied %d bytes into socket", n)
						if scheme == "unixgram" {
							// Write zero bytes after Stop is called to signal that this is the "end of the stream".
							for {
								_, err = s.Write([]byte{})
								if err == nil {
									glog.Infof("Zero bytes written to socket to signal EOF")
									break
								}
								var netErr net.Error
								if errors.As(err, &netErr) && netErr.Timeout() {
									glog.Infof("Write timeout")
									time.Sleep(1 * time.Second)
								} else {
									testutil.FatalIfErr(t, err)
								}
							}
						}
						source.Close()
						s.Close()
					}()

					// Oneshot mode means we can wait for shutdown before cancelling.
					wg.Wait()
					cancel()

					var sockMetrics, fileMetrics metrics.MetricSlice
					sockStore.Range(func(m *metrics.Metric) error {
						sockMetrics = append(sockMetrics, m)
						return nil
					})
					fileStore.Range(func(m *metrics.Metric) error {
						fileMetrics = append(fileMetrics, m)
						return nil
					})

					// Ignore the datum.Time field as well, as the results will be unstable otherwise.
					testutil.ExpectNoDiff(t, fileMetrics, sockMetrics, testutil.SortSlices(metrics.Less), testutil.IgnoreUnexported(metrics.Metric{}, sync.RWMutex{}, datum.String{}), testutil.IgnoreFields(datum.BaseDatum{}, "Time"))
				}))
		}
	}
}
