// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build integration

package mtail_test

import (
	"fmt"
	"path"
	"sync"
	"testing"
	"time"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestBasicTail(t *testing.T) {
	tests := []struct {
		pollInterval   time.Duration
		enableFsNotify bool
	}{
		{0, true},                      // notify only
		{10 * time.Millisecond, false}, // poll only
	}
	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "tail=2,log_watcher=2")()
	}
	for _, test := range tests {
		t.Run(fmt.Sprintf("%s %v", test.pollInterval, test.enableFsNotify), func(t *testing.T) {
			logDir, rmLogDir := testutil.TestTempDir(t)
			defer rmLogDir()

			m, stopM := mtail.TestStartServer(t, test.pollInterval, test.enableFsNotify, mtail.LogPathPatterns(logDir+"/*"), mtail.ProgramPath("../../examples/linecount.mtail"))
			defer stopM()

			startLineCount := mtail.TestGetMetric(t, m.Addr(), "lines_total")
			startLogCount := mtail.TestGetMetric(t, m.Addr(), "log_count")

			time.Sleep(1 * time.Second)

			logFile := path.Join(logDir, "log")

			f := testutil.TestOpenFile(t, logFile)

			for i := 1; i <= 3; i++ {
				testutil.WriteString(t, f, fmt.Sprintf("%d\n", i))
				time.Sleep(1 * time.Second)
			}

			var wg sync.WaitGroup
			wg.Add(2)
			go func() {
				defer wg.Done()
				check := func() (bool, error) {
					end := mtail.TestGetMetric(t, m.Addr(), "lines_total")
					return mtail.TestMetricDelta(end, startLineCount) == 3, nil
				}
				ok, err := testutil.DoOrTimeout(check, 10*time.Second, 10*time.Millisecond)
				if err != nil {
					t.Fatal(err)
				}
				if !ok {
					t.Error()
				}
			}()
			go func() {
				defer wg.Done()
				check := func() (bool, error) {
					end := mtail.TestGetMetric(t, m.Addr(), "log_count")
					return mtail.TestMetricDelta(end, startLogCount) == 1, nil
				}
				ok, err := testutil.DoOrTimeout(check, 10*time.Second, 100*time.Millisecond)
				if err != nil {

					t.Fatal(err)
				}
				if !ok {
					t.Error(err)
				}
			}()
			wg.Wait()
		})
	}
}
