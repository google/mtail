// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package tailer provides a class that is responsible for tailing log files
// and extracting new log lines to be passed into the virtual machines.
package tailer

import (
	"context"
	"expvar"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sync"
	"time"

	"github.com/golang/glog"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/waker"
)

var (
	// logCount records the number of logs that are being tailed
	logCount = expvar.NewInt("log_count")
)

// Tailer polls for new logs matching a pattern and sets up log streams from
// each of those logs.
type Tailer struct {
	ctx context.Context
	llp logline.Processor

	initDone chan struct{}

	globPatternsMu     sync.RWMutex        // protects `globPatterns'
	globPatterns       map[string]struct{} // glob patterns to match newly created logs in dir paths against
	ignoreRegexPattern *regexp.Regexp

	oneShot bool

	pollMu sync.Mutex // protects Poll()

	logstreamPollWaker waker.Waker                    // Used for waking idle logstreams
	wg                 sync.WaitGroup                 // count active logstreams, wait for shutdown.
	logstreamsMu       sync.RWMutex                   // protects `logstreams`.
	logstreams         map[string]logstream.LogStream // Map absolte pathname to logstream reading that pathname.

}

// Option configures a new Tailer.
type Option interface {
	apply(*Tailer) error
}

type niladicOption struct {
	applyfunc func(*Tailer) error
}

func (n *niladicOption) apply(t *Tailer) error {
	return n.applyfunc(t)
}

// OneShot puts the tailer in one-shot mode, where sources are read once from the start and then closed.
var OneShot = &niladicOption{func(t *Tailer) error { t.oneShot = true; return nil }}

// LogPatterns sets the glob patterns to use to match pathnames.
type LogPatterns []string

func (opt LogPatterns) apply(t *Tailer) error {
	for _, p := range opt {
		if err := t.AddPattern(p); err != nil {
			return err
		}
	}
	return nil
}

// IgnoreRegex sets the regular expression to use to filter away pathnames that match the LogPatterns glob
type IgnoreRegex string

func (opt IgnoreRegex) apply(t *Tailer) error {
	t.SetIgnorePattern(string(opt))
	return nil
}

// StaleLogGcWaker triggers garbage collection runs for stale logs in the tailer.
func StaleLogGcWaker(w waker.Waker) Option {
	return &staleLogGcWaker{w}
}

type staleLogGcWaker struct {
	waker.Waker
}

func (opt staleLogGcWaker) apply(t *Tailer) error {
	t.StartGcLoop(opt.Waker)
	return nil
}

// LogPatternPollWaker triggers polls on the filesystem for new logs that match the log glob patterns.
func LogPatternPollWaker(w waker.Waker) Option {
	return &logPatternPollWaker{w}
}

type logPatternPollWaker struct {
	waker.Waker
}

func (opt logPatternPollWaker) apply(t *Tailer) error {
	t.StartLogPatternPollLoop(opt.Waker)
	return nil
}

// LogstreamPollWaker wakes idle logstreams.
func LogstreamPollWaker(w waker.Waker) Option {
	return &logstreamPollWaker{w}
}

type logstreamPollWaker struct {
	waker.Waker
}

func (opt logstreamPollWaker) apply(t *Tailer) error {
	t.logstreamPollWaker = opt.Waker
	return nil
}

// New creates a new Tailer.
func New(ctx context.Context, llp logline.Processor, options ...Option) (*Tailer, error) {
	t := &Tailer{
		ctx:          ctx,
		llp:          llp,
		initDone:     make(chan struct{}),
		globPatterns: make(map[string]struct{}),
		logstreams:   make(map[string]logstream.LogStream),
	}
	defer close(t.initDone)
	if err := t.SetOption(options...); err != nil {
		return nil, err
	}
	// Guarantee one poll of the filesystem by the time we return.
	if err := t.PollLogPatterns(); err != nil {
		return nil, err
	}
	return t, nil
}

// SetOption takes one or more option functions and applies them in order to Tailer.
func (t *Tailer) SetOption(options ...Option) error {
	for _, option := range options {
		if err := option.apply(t); err != nil {
			return err
		}
	}
	return nil
}

// AddPattern adds a pattern to the list of patterns to filter filenames against.
func (t *Tailer) AddPattern(pattern string) error {
	absPath, err := filepath.Abs(pattern)
	if err != nil {
		glog.V(2).Infof("Couldn't canonicalize path %q: %s", pattern, err)
		return err
	}
	glog.V(2).Infof("AddPattern: %s", absPath)
	t.globPatternsMu.Lock()
	t.globPatterns[absPath] = struct{}{}
	t.globPatternsMu.Unlock()
	return nil
}

func (t *Tailer) Ignore(pathname string) (bool, error) {
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		return false, err
	}
	fi, err := os.Stat(absPath)
	if err != nil {
		return false, err
	}
	if fi.Mode().IsDir() {
		glog.V(2).Infof("ignore path %q because it is a folder", pathname)
		return true, nil
	}
	return t.ignoreRegexPattern != nil && t.ignoreRegexPattern.MatchString(fi.Name()), nil
}

func (t *Tailer) SetIgnorePattern(pattern string) error {
	if len(pattern) == 0 {
		return nil
	}
	glog.V(2).Infof("Set filename ignore regex pattern %q", pattern)
	ignoreRegexPattern, err := regexp.Compile(pattern)
	if err != nil {
		glog.V(2).Infof("Couldn't compile regex %q: %s", pattern, err)
		fmt.Println(fmt.Sprintf("error: %v", err))
		return err
	}
	t.ignoreRegexPattern = ignoreRegexPattern
	return nil
}

// TailPath registers a filesystem pathname to be tailed.
func (t *Tailer) TailPath(pathname string) error {
	t.logstreamsMu.Lock()
	defer t.logstreamsMu.Unlock()
	if l, ok := t.logstreams[pathname]; ok {
		if !l.IsComplete() {
			glog.V(2).Infof("already got a logstream on %q", pathname)
			return nil
		}
		logCount.Add(-1) // Removing the current entry before re-adding.
		glog.V(2).Infof("Existing logstream is finished, creating a new one.")
	}
	l, err := logstream.New(t.ctx, &t.wg, t.logstreamPollWaker, pathname, t.llp, t.oneShot)
	if err != nil {
		return err
	}
	if t.oneShot {
		l.Stop()
	}
	t.logstreams[pathname] = l
	glog.Infof("Tailing %s", pathname)
	logCount.Add(1)
	return nil
}

// Close assumes that the context is already cancelled and waits for logstreams to shut down.
func (t *Tailer) Close() error {
	glog.Infof("Waiting for logstream shutdown")
	t.wg.Wait()
	return nil
}

// Gc removes logstreams that have had no reads for 24h or more.
func (t *Tailer) Gc() error {
	t.logstreamsMu.Lock()
	defer t.logstreamsMu.Unlock()
	for k, v := range t.logstreams {
		if time.Since(v.LastReadTime()) > (time.Hour * 24) {
			v.Stop()
		}
		if v.IsComplete() {
			delete(t.logstreams, k)
		}
	}
	return nil
}

// StartGcLoop runs a permanent goroutine to expire metrics every duration.
func (t *Tailer) StartGcLoop(waker waker.Waker) {
	if waker == nil {
		glog.Info("Log handle expiration disabled")
		return
	}
	go func() {
		<-t.initDone
		//glog.Infof("Starting log handle expiry loop every %s", duration.String())
		for {
			select {
			case <-t.ctx.Done():
				return
			case <-waker.Wake():
				if err := t.Gc(); err != nil {
					glog.Info(err)
				}
			}
		}
	}()
}

// StartLogPatternPollLoop runs a permanent goroutine to poll for new log files.
func (t *Tailer) StartLogPatternPollLoop(waker waker.Waker) {
	if waker == nil {
		glog.Info("Log pattern polling disabled")
		return
	}
	go func() {
		<-t.initDone
		//glog.Infof("Starting log pattern poll loop every %s", duration.String())
		for {
			select {
			case <-t.ctx.Done():
				return
			case <-waker.Wake():
				if err := t.Poll(); err != nil {
					glog.Info(err)
				}
			}
		}
	}()
}

func (t *Tailer) PollLogPatterns() error {
	t.globPatternsMu.RLock()
	defer t.globPatternsMu.RUnlock()
	for pattern := range t.globPatterns {
		matches, err := filepath.Glob(pattern)
		if err != nil {
			return err
		}
		glog.V(1).Infof("glob matches: %v", matches)
		for _, pathname := range matches {
			ignore, err := t.Ignore(pathname)
			if err != nil {
				return err
			}
			if ignore {
				continue
			}
			absPath, err := filepath.Abs(pathname)
			if err != nil {
				return err
			}
			glog.Infof("watched path is %q", absPath)
			if err := t.TailPath(absPath); err != nil {
				glog.Info(err)
			}
		}
	}
	return nil
}

func (t *Tailer) Poll() error {
	t.pollMu.Lock()
	defer t.pollMu.Unlock()
	if err := t.PollLogPatterns(); err != nil {
		return err
	}
	return nil
}
