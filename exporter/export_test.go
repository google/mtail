// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"bytes"
	"io/ioutil"
	"net"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/metrics"
	"github.com/kylelemons/godebug/pretty"
)

func FakeSocketWrite(f formatter, m *metrics.Metric) []string {
	var ret []string
	lc := make(chan *metrics.LabelSet)
	go m.EmitLabelSets(lc)
	for l := range lc {
		ret = append(ret, f(m, l))
	}
	sort.Strings(ret)
	return ret
}

func TestMetricToCollectd(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}
	hostname, herr := os.Hostname()
	if herr != nil {
		t.Errorf("hostname error: %s", herr)
	}
	ms := metrics.Store{}
	e := New(&ms)

	scalarMetric := metrics.NewMetric("foo", "prog", metrics.Counter)
	d, _ := scalarMetric.GetDatum()
	d.Set(37, ts)
	ms.Add(scalarMetric)

	r := newRecordingSocketListener(t)
	if err := e.CollectdWriteMetrics(r.Addr); err != nil {
		t.Errorf("Write failed: %s", err)
	}

	expected := []string{"PUTVAL \"" + hostname + "/mtail-prog/counter-foo\" interval=60 1343124840:37\n"}
	diff := pretty.Compare(r.Record.String(), strings.Join(expected, ""))
	if len(diff) > 0 {
		t.Errorf("String didn't match:\n%s", diff)
	}
	r.Close()

	dimensionedMetric := metrics.NewMetric("bar", "prog", metrics.Gauge, "label")
	d, _ = dimensionedMetric.GetDatum("quux")
	d.Set(37, ts)
	d, _ = dimensionedMetric.GetDatum("snuh")
	d.Set(37, ts)
	ms.ClearMetrics()
	ms.Add(dimensionedMetric)

	r = newRecordingSocketListener(t)
	if err := e.CollectdWriteMetrics(r.Addr); err != nil {
		t.Errorf("Write failed: %s", err)
	}
	expected = []string{
		"PUTVAL \"" + hostname + "/mtail-prog/gauge-bar-label-quux\" interval=60 1343124840:37\n",
		"PUTVAL \"" + hostname + "/mtail-prog/gauge-bar-label-snuh\" interval=60 1343124840:37\n"}
	diff = pretty.Compare(r.Record.String(), strings.Join(expected, ""))
	if len(diff) > 0 {
		t.Errorf("String didn't match:\n%s", diff)
	}
	r.Close()
}

func TestMetricToGraphite(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}

	scalarMetric := metrics.NewMetric("foo", "prog", metrics.Counter)
	d, _ := scalarMetric.GetDatum()
	d.Set(37, ts)
	r := FakeSocketWrite(metricToGraphite, scalarMetric)
	expected := []string{"prog.foo 37 1343124840\n"}
	diff := pretty.Compare(r, expected)
	if len(diff) > 0 {
		t.Errorf("String didn't match:\n%s", diff)
	}

	dimensionedMetric := metrics.NewMetric("bar", "prog", metrics.Gauge, "l")
	d, _ = dimensionedMetric.GetDatum("quux")
	d.Set(37, ts)
	d, _ = dimensionedMetric.GetDatum("snuh")
	d.Set(37, ts)
	r = FakeSocketWrite(metricToGraphite, dimensionedMetric)
	expected = []string{
		"prog.bar.l.quux 37 1343124840\n",
		"prog.bar.l.snuh 37 1343124840\n"}
	diff = pretty.Compare(r, expected)
	if len(diff) > 0 {
		t.Errorf("String didn't match:\n%s", diff)
	}
}

func TestMetricToStatsd(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}

	scalarMetric := metrics.NewMetric("foo", "prog", metrics.Counter)
	d, _ := scalarMetric.GetDatum()
	d.Set(37, ts)
	r := FakeSocketWrite(metricToStatsd, scalarMetric)
	expected := []string{"prog.foo:37|c"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	dimensionedMetric := metrics.NewMetric("bar", "prog", metrics.Gauge, "l")
	d, _ = dimensionedMetric.GetDatum("quux")
	d.Set(37, ts)
	d, _ = dimensionedMetric.GetDatum("snuh")
	d.Set(42, ts)
	r = FakeSocketWrite(metricToStatsd, dimensionedMetric)
	expected = []string{
		"prog.bar.l.quux:37|c",
		"prog.bar.l.snuh:42|c"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}
}

type RecordingListener struct {
	net.Listener
	t      *testing.T
	dir    string
	Addr   string
	Record bytes.Buffer
}

func newRecordingTCPListener(t *testing.T) *RecordingListener {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	addr := "127.0.0.1:0"
	if err != nil {
		l, err = net.Listen("tcp6", "[::1]:0")
		addr = "[::1]:0"
	}
	if err != nil {
		t.Fatal(err)
	}
	r := &RecordingListener{Listener: l, t: t, Addr: addr}
	go r.run()
	return r
}

func newRecordingSocketListener(t *testing.T) *RecordingListener {
	dir, err := ioutil.TempDir("", "export_test")
	if err != nil {
		t.Fatal(err)
	}
	addr := filepath.Join(dir, "socket")
	l, err := net.Listen("unix", addr)
	if err != nil {
		t.Fatalf("can't listen at %s: %s", addr, err)
	}
	r := &RecordingListener{Listener: l, dir: dir, t: t, Addr: addr}
	go r.run()
	return r
}

func (r *RecordingListener) run() {
	conn, err := r.Accept()
	if err != nil {
		r.t.Logf("error accepting: %s", err)
		return
	}
	go func(c net.Conn) {
		defer c.Close()
		r.t.Logf("reading")
		n, err := r.Record.ReadFrom(c)
		r.t.Logf("%d bytes read", n)
		if err != nil {
			r.t.Fatalf("error reading: %s", err)
			return
		}
		r.t.Logf("%d bytes read", n)
	}(conn)
}

func (r *RecordingListener) Close() {
	if r.dir != "" {
		if err := os.RemoveAll(r.Addr); err != nil {
			r.t.Fatalf("failed to remove socket: %s")
		}
	}
	r.Listener.Close()
}
