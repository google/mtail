// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"bufio"
	"expvar"
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

var vmTests = []struct {
	name string
	prog string
	log  string
}{
	{"single-dash-parseint",
		`counter c

/(?P<x>-)/ {
    $x == "-" {
        c++
    }
}
`, `123 a
- b
`,
	},
}

func TestVmEndToEnd(t *testing.T) {
	t.Skip("busted")
	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "vm=2,loader=2")()
	}
	for _, tc := range vmTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			w := watcher.NewFakeWatcher()
			lines := make(chan *logline.LogLine, 0)
			store := metrics.NewStore()
			l, err := NewLoader("", store, lines, w, ErrorsAbort, DumpAst, DumpAstTypes, DumpBytecode, OmitMetricSource)
			testutil.FatalIfErr(t, err)
			compileErrors := l.CompileAndRun(tc.name, strings.NewReader(tc.prog))
			testutil.FatalIfErr(t, compileErrors)
			scanner := bufio.NewScanner(strings.NewReader(tc.log))
			lineCount := 0
			for scanner.Scan() {
				lineCount++
				lines <- logline.New(tc.name, scanner.Text())
			}
			close(lines)
			w.Close()

			check := func() (bool, error) {
				if expvar.Get("line_count").String() != string(lineCount) {
					return false, nil
				}
				return true, nil
			}
			ok, err := testutil.DoOrTimeout(check, 100*time.Millisecond, 10*time.Millisecond)
			if err != nil {
				t.Errorf("Busted, timeout waiting for line count.")
			}

			if !ok {
				t.Errorf("Incorrect lines read: expected %d received %s", lineCount, expvar.Get("line_count"))
			}
			// This is not good.
			if expvar.Get("prog_runtime_errors").(*expvar.Map).Get(tc.name).String() != "0" {
				t.Errorf("Nonzero runtime errors from program: got %s", expvar.Get("prog_runtime_errors").(*expvar.Map).Get(tc.name))
			}
			t.Logf("Store is %v", store)
		})
	}
}
