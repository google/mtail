// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"expvar"
	"fmt"
	"runtime"
	"strings"
	"testing"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

const testProgram = "/$/ { }\n"

func startMtailServer(t *testing.T, options ...func(*Server) error) *Server {
	expvar.Get("lines_total").(*expvar.Int).Set(0)
	expvar.Get("log_count").(*expvar.Int).Set(0)
	expvar.Get("log_rotations_total").(*expvar.Map).Init()
	expvar.Get("prog_loads_total").(*expvar.Map).Init()

	w, err := watcher.NewLogWatcher(0, true)
	if err != nil {
		t.Errorf("Couodn't make a log watcher: %s", err)
	}
	m, err := New(metrics.NewStore(), w, options...)
	testutil.FatalIfErr(t, err)
	if pErr := m.l.CompileAndRun("test", strings.NewReader(testProgram)); pErr != nil {
		t.Errorf("Couldn't compile program: %s", pErr)
	}

	if err := m.StartTailing(); err != nil {
		t.Errorf("StartTailing failed: %s", err)
	}
	return m
}

func TestBuildInfo(t *testing.T) {
	buildInfo := BuildInfo{
		Branch:   "foo",
		Version:  "bar",
		Revision: "baz",
	}

	buildInfoWant := fmt.Sprintf(
		"mtail version bar git revision baz go version %s go arch %s go os %s",
		runtime.Version(),
		runtime.GOARCH,
		runtime.GOOS,
	)
	buildInfoGot := buildInfo.String()

	if buildInfoWant != buildInfoGot {
		t.Errorf("Unexpected build info string, want: %q, got: %q", buildInfoWant, buildInfoGot)
	}
}
