// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"io"
	"os"
	"strings"
	"testing"
)

var logProcessingBenchmarks = []struct {
	progfile string
	logfile  string
}{
	{
		"examples/rsyncd.em",
		"testdata/rsyncd.log",
	},
	{
		"examples/linecount.em",
		"testdata/linecount.log",
	},
}

func BenchmarkLogProcessing(b *testing.B) {
TestLoop:
	for _, bc := range logProcessingBenchmarks {
		b.StopTimer()
		metrics = make([]*Metric, 0)

		p, err := os.Open(bc.progfile)
		if err != nil {
			b.Errorf("%s: could not open program file: %s", bc.progfile, err)
			continue
		}
		defer p.Close()

		v, errs := Compile(bc.progfile, p)
		if errs != nil {
			b.Errorf("%s: compile failed: %s", bc.progfile, strings.Join(errs, "\n"))
			continue
		}

		l, err := os.Open(bc.logfile)
		if err != nil {
			b.Errorf("%s: could not open log file: %s", bc.logfile, err)
			continue
		}
		defer l.Close()

		var lines chan string
		lines = make(chan string)
		var vms = []*vm{v}
		go RunVms(vms, lines)

		r := bufio.NewReader(l)

		b.StartTimer()

		for i := 0; i < b.N; i++ {

		ReadLoop:
			for {
				line, err := r.ReadString('\n')

				switch {
				case err == io.EOF:
					break ReadLoop
				case err != nil:
					b.Errorf("%s: read error: %s", bc.progfile, err)
					continue TestLoop
				default:
					lines <- line
				}
			}
		}
	}
}
