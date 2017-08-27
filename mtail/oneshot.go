// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
	"github.com/pkg/errors"
)

// OneShot reads the contents of a log file into the lines channel from start to finish, terminating the program at the end.
func (m *MtailServer) OneShot(logfile string, print bool) (count int64, err error) {
	glog.Infof("Oneshot %q", logfile)
	l, err := m.o.FS.Open(logfile)
	if err != nil {
		return 0, errors.Wrapf(err, "failed to open log file %q", logfile)
	}
	defer l.Close()

	r := bufio.NewReader(l)

	if print {
		fmt.Printf("%s: %d MAXPROCS, %d CPUs, ", logfile, runtime.GOMAXPROCS(-1), runtime.NumCPU())
	}

	start := time.Now()

Loop:
	for {
		line, err := r.ReadString('\n')
		line = strings.TrimSuffix(line, "\n")
		switch {
		case err == io.EOF:
			if len(line) > 0 {
				m.lines <- tailer.NewLogLine(logfile, line)
			}
			break Loop
		case err != nil:
			return 0, errors.Wrapf(err, "failed to read from %q", logfile)
		default:
			m.lines <- tailer.NewLogLine(logfile, line)
		}
	}
	duration := time.Since(start)
	count, err = strconv.ParseInt(vm.LineCount.String(), 10, 64)
	if err != nil {
		return
	}
	if print {
		µsPerL := float64(duration.Nanoseconds()) / (float64(count) * 1000)
		fmt.Printf("%d lines, %s total time, %6.3f µs/line\n", count, duration, µsPerL)
	}
	return
}

// RunOneShot performs the work of the one_shot commandline flag; after compiling programs mtail will read all of the log files in full, once, dump the metric results at the end, and then exit.
func (m *MtailServer) RunOneShot() {
	fmt.Println("Oneshot results:")
	for _, pathname := range m.o.LogPathPatterns {
		_, err := m.OneShot(pathname, true)
		if err != nil {
			glog.Exitf("Failed one shot mode for %q: %s\n", pathname, err)
		}
	}
	if m.o.OneShotMetrics {
		fmt.Printf("Metrics store:")
		if err := m.WriteMetrics(os.Stdout); err != nil {
			glog.Exit(err)
		}
	}
	m.Close()
}
