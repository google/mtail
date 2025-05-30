// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package exporter provides the interface for getting metrics out of mtail,
// into your monitoring system of choice.
package exporter

import (
	"context"
	"expvar"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/pkg/errors"
)

// Commandline Flags.
var (
	writeDeadline = flag.Duration("metric_push_write_deadline", 10*time.Second, "Time to wait for a push to succeed before exiting with an error.")
	enableOpenTelemetry = flag.Bool("experimental_enable_opentelemetry", false, "Enable the experimental Open Telemetry metric pusher.")
)

// Exporter manages the export of metrics to passive and active collectors.
type Exporter struct {
	ctx            context.Context
	cancelFunc     context.CancelFunc
	wg             sync.WaitGroup
	store          *metrics.Store
	pushInterval   time.Duration
	hostname       string
	version string
	omitProgLabel  bool
	emitTimestamp  bool
	exportDisabled bool
	pushTargets    []pushOptions
	initDone       chan struct{}
	shutdownDone   chan struct{}
}

// Option configures a new Exporter.
type Option func(*Exporter) error

// Hostname specifies the mtail hostname to use in exported metrics.
func Hostname(hostname string) Option {
	return func(e *Exporter) error {
		e.hostname = hostname
		return nil
	}
}

// Version specifies the mtail version to use in exported metrics.
func Version(version string) Option {
	return func (e *Exporter) error {
		e.version = version
		return nil
	}
}

// OmitProgLabel sets the Exporter to not put program names in metric labels.
func OmitProgLabel() Option {
	return func(e *Exporter) error {
		e.omitProgLabel = true
		return nil
	}
}

// EmitTimestamp instructs the exporter to send metric's timestamps to collectors.
func EmitTimestamp() Option {
	return func(e *Exporter) error {
		e.emitTimestamp = true
		return nil
	}
}

func PushInterval(opt time.Duration) Option {
	return func(e *Exporter) error {
		e.pushInterval = opt
		return nil
	}
}

func DisableExport() Option {
	return func(e *Exporter) error {
		e.exportDisabled = true
		return nil
	}
}

var ErrNeedsStore = errors.New("exporter needs a Store")

// New creates a new Exporter.
func New(ctx context.Context, store *metrics.Store, options ...Option) (*Exporter, error) {
	if store == nil {
		return nil, ErrNeedsStore
	}
	e := &Exporter{
		store:        store,
		initDone:     make(chan struct{}),
		shutdownDone: make(chan struct{}),
	}
	e.ctx, e.cancelFunc = context.WithCancel(ctx)
	defer close(e.initDone)
	if err := e.SetOption(options...); err != nil {
		return nil, err
	}
	// defaults after options have been set
	if e.hostname == "" {
		var err error
		e.hostname, err = os.Hostname()
		if err != nil {
			return nil, errors.Wrap(err, "getting hostname")
		}
	}

	if *collectdSocketPath != "" {
		o := pushOptions{"unix", *collectdSocketPath, metricToCollectd, collectdExportTotal, collectdExportSuccess}
		e.RegisterPushExport(o)
	}
	if *graphiteHostPort != "" {
		o := pushOptions{"tcp", *graphiteHostPort, metricToGraphite, graphiteExportTotal, graphiteExportSuccess}
		e.RegisterPushExport(o)
	}
	if *statsdHostPort != "" {
		o := pushOptions{"udp", *statsdHostPort, metricToStatsd, statsdExportTotal, statsdExportSuccess}
		e.RegisterPushExport(o)
	}
	if *enableOpenTelemetry {
		err := e.InitOtel(ctx)
		if err != nil {
			return nil, err
		}
	}
	e.StartMetricPush()

	// This routine manages shutdown of the Exporter.
	go func() {
		<-e.initDone
		// Wait for the context to be completed before waiting for subroutines.
		if !e.exportDisabled {
			<-e.ctx.Done()
		}
		e.wg.Wait()
		close(e.shutdownDone)
	}()
	return e, nil
}

// Stop instructs the exporter to shut down.  The function returns once the exporter has finished.
func (e *Exporter) Stop() {
	e.cancelFunc()
	<-e.shutdownDone
}

// SetOption takes one or more option functions and applies them in order to Exporter.
func (e *Exporter) SetOption(options ...Option) error {
	for _, option := range options {
		if err := option(e); err != nil {
			return err
		}
	}
	return nil
}

// formatLabels converts a metric name and key-value map of labels to a single
// string for exporting to the correct output format for each export target.
// ksep and sep mark what to use for key/val separator, and between label separators respoectively.
// If not empty, rep is used to replace cases of ksep and sep in the original strings.
func formatLabels(name string, m map[string]string, ksep, sep, rep string) string {
	r := name
	if len(m) > 0 {
		var keys []string
		for k := range m {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		var s []string
		for _, k := range keys {
			k1 := strings.ReplaceAll(strings.ReplaceAll(k, ksep, rep), sep, rep)
			v1 := strings.ReplaceAll(strings.ReplaceAll(m[k], ksep, rep), sep, rep)
			s = append(s, fmt.Sprintf("%s%s%s", k1, ksep, v1))
		}
		return r + sep + strings.Join(s, sep)
	}
	return r
}

// Format a LabelSet into a string to be written to one of the timeseries
// sockets.
type formatter func(string, *metrics.Metric, *metrics.LabelSet, time.Duration) string

func (e *Exporter) writeSocketMetrics(c io.Writer, f formatter, exportTotal *expvar.Int, exportSuccess *expvar.Int) error {
	return e.store.Range(func(m *metrics.Metric) error {
		m.RLock()
		// Don't try to send text metrics to any push service.
		if m.Kind == metrics.Text {
			m.RUnlock()
			return nil
		}
		exportTotal.Add(1)
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := f(e.hostname, m, l, e.pushInterval)
			n, err := fmt.Fprint(c, line)
			glog.V(2).Infof("Sent %d bytes\n", n)
			if err == nil {
				exportSuccess.Add(1)
			} else {
				return errors.Errorf("write error: %s", err)
			}
		}
		m.RUnlock()
		return nil
	})
}

// PushMetrics sends metrics to each of the configured services.
func (e *Exporter) PushMetrics() {
	for _, target := range e.pushTargets {
		glog.V(2).Infof("pushing to %s", target.addr)
		conn, err := net.DialTimeout(target.net, target.addr, *writeDeadline)
		if err != nil {
			glog.Infof("pusher dial error: %s", err)
			continue
		}
		err = conn.SetDeadline(time.Now().Add(*writeDeadline))
		if err != nil {
			glog.Infof("Couldn't set deadline on connection: %s", err)
		}
		err = e.writeSocketMetrics(conn, target.f, target.total, target.success)
		if err != nil {
			glog.Infof("pusher write error: %s", err)
		}
		err = conn.Close()
		if err != nil {
			glog.Infof("connection close failed: %s", err)
		}
	}
}

// StartMetricPush pushes metrics to the configured services each interval.
func (e *Exporter) StartMetricPush() {
	if e.exportDisabled {
		glog.Info("Export loop disabled.")
		return
	}
	if len(e.pushTargets) == 0 {
		return
	}
	if e.pushInterval <= 0 {
		return
	}
	e.wg.Add(1)
	go func() {
		defer e.wg.Done()
		<-e.initDone
		glog.Info("Started metric push.")
		ticker := time.NewTicker(e.pushInterval)
		defer ticker.Stop()
		for {
			select {
			case <-e.ctx.Done():
				return
			case <-ticker.C:
				e.PushMetrics()
			}
		}
	}()
}

type pushOptions struct {
	net, addr      string
	f              formatter
	total, success *expvar.Int
}

// RegisterPushExport adds a push export connection to the Exporter.  Items in
// the list must describe a Dial()able connection and will have all the metrics
// pushed to each pushInterval.
func (e *Exporter) RegisterPushExport(p pushOptions) {
	e.pushTargets = append(e.pushTargets, p)
}
