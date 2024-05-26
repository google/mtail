// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

//go:build unix

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
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
	"golang.org/x/sys/unix"
)

// TestFilePipeStreamComparison is a unix-specific test since unix.Mkfifo is not defined on Windows.
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
				testutil.FatalIfErr(t, unix.Mkfifo(pipeName, 0o600))

				var wg sync.WaitGroup
				wg.Add(3)
				// This goroutine copies bytes from the source file into the
				// fifo, once the fifo has been opened for read.
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

				// Two mtails both alike in dignity.
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
				go func() {
					defer wg.Done()
					pipeMtail, err := mtail.New(ctx, pipeStore, mtail.ProgramPath(programFile), mtail.LogPathPatterns(pipeName), mtail.OneShot, mtail.OmitMetricSource, mtail.LogPatternPollWaker(waker), mtail.LogstreamPollWaker(waker))
					testutil.FatalIfErr(t, err)
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

// TestFileSocketStreamComparison is a unix-specific test currently because on Windows, the constructed URL will
// be of the form unix://C:\\path, and this will be interpreted as protocol unix on host C and port \\path.
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
						s, err := net.DialUnix(scheme, nil, &net.UnixAddr{Name: sockName, Net: scheme})
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
