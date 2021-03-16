// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package tailer provides a class that is responsible for tailing log files
// and extracting new log lines to be passed into the virtual machines.
package tailer

import (
	"context"
	"errors"
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

// Tailer polls the filesystem for log sources that match given
// `LogPathPatterns` and creates `LogStream`s to tail them.
type Tailer struct {
	ctx   context.Context
	wg    sync.WaitGroup // Wait for our subroutines to finish
	lines chan<- *logline.LogLine

	globPatternsMu     sync.RWMutex        // protects `globPatterns'
	globPatterns       map[string]struct{} // glob patterns to match newly created logs in dir paths against
	ignoreRegexPattern *regexp.Regexp

	logSocketsMu sync.RWMutex        // protects `logSockets'
	logSockets   map[string]struct{} // which hostspecs do we bind to

	oneShot bool

	pollMu sync.Mutex // protects Poll()

	logstreamPollWaker waker.Waker                    // Used for waking idle logstreams
	logstreamsMu       sync.RWMutex                   // protects `logstreams`.
	logstreams         map[string]logstream.LogStream // Map absolte pathname to logstream reading that pathname.

	initDone chan struct{}
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

// LogSockets sets the glob patterns to use to match pathnames.
type LogSockets []string

func (opt LogSockets) apply(t *Tailer) error {
	for _, u := range opt {
		if err := t.AddSocket(u); err != nil {
			return err
		}
	}
	return nil
}

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
func New(ctx context.Context, wg *sync.WaitGroup, lines chan<- *logline.LogLine, options ...Option) (*Tailer, error) {
	if lines == nil {
		return nil, errors.New("Tailer needs a lines channel")
	}
	t := &Tailer{
		ctx:          ctx,
		lines:        lines,
		initDone:     make(chan struct{}),
		globPatterns: make(map[string]struct{}),
		logSockets:   make(map[string]struct{}),
		logstreams:   make(map[string]logstream.LogStream),
	}
	defer close(t.initDone)
	if err := t.SetOption(options...); err != nil {
		return nil, err
	}
	if len(t.globPatterns) == 0 && len(t.logSockets) == 0 {
		glog.Info("No patterns or sockets to tail, tailer done.")
		close(t.lines)
		return t, nil
	}
	// Guarantee all existing logs get tailed before we leave.  Also necessary
	// in case oneshot mode is active, the logs get read!
	if err := t.PollLogSockets(); err != nil {
		return nil, err
	}
	if err := t.PollLogPatterns(); err != nil {
		return nil, err
	}
	// Setup for shutdown, once all routines are finished.
	wg.Add(1)
	go func() {
		defer wg.Done()
		<-t.initDone
		// We need to wait for context.Done() before we wait for the subbies
		// because we don't know how many are running at any point -- as soon
		// as t.wg.Wait begins the number of waited-on goroutines is fixed, and
		// we may end up leaking a LogStream goroutine and it'll try to send on
		// a closed channel as a result.  But in tests and oneshot, we want to
		// make sure the whole log gets read so we can't wait on context.Done
		// here.
		if !t.oneShot {
			<-t.ctx.Done()
		}
		t.wg.Wait()
		close(t.lines)
	}()
	return t, nil
}

var ErrNilOption = errors.New("nil option supplied")

// SetOption takes one or more option functions and applies them in order to Tailer.
func (t *Tailer) SetOption(options ...Option) error {
	for _, option := range options {
		if option == nil {
			return ErrNilOption
		}
		if err := option.apply(t); err != nil {
			return err
		}
	}
	return nil
}

// AddSocket adds a socket to the list of hosts and ports to listen for log events.
func (t *Tailer) AddSocket(pathname string) error {
	glog.V(2).Infof("AddSocket: %s", pathname)
	t.logSocketsMu.Lock()
	t.logSockets[pathname] = struct{}{}
	t.logSocketsMu.Unlock()
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
	l, err := logstream.New(t.ctx, &t.wg, t.logstreamPollWaker, pathname, t.lines, t.oneShot)
	if err != nil {
		return err
	}
	if t.oneShot {
		glog.V(2).Infof("Starting oneshot read at startup of %q", pathname)
		l.Stop()
	}
	t.logstreams[pathname] = l
	glog.Infof("Tailing %s", pathname)
	logCount.Add(1)
	return nil
}

// Gc removes logstreams that have had no reads for 24h or more.
func (t *Tailer) Gc() error {
	t.logstreamsMu.Lock()
	defer t.logstreamsMu.Unlock()
	for _, v := range t.logstreams {
		if time.Since(v.LastReadTime()) > (time.Hour * 24) {
			v.Stop()
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
	t.wg.Add(1)
	go func() {
		defer t.wg.Done()
		<-t.initDone
		if t.oneShot {
			glog.Info("No gc loop in oneshot mode.")
			return
		}
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
	t.wg.Add(1)
	go func() {
		defer t.wg.Done()
		<-t.initDone
		if t.oneShot {
			glog.Info("No polling loop in oneshot mode.")
			return
		}
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

func (t *Tailer) PollLogSockets() error {
	t.logSocketsMu.RLock()
	defer t.logSocketsMu.RUnlock()
	for socket := range t.logSockets {
		glog.V(2).Infof("watched path is %q", socket)
		if err := t.TailPath(socket); err != nil {
			glog.Info(err)
		}
	}
	return nil
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
			glog.V(2).Infof("watched path is %q", absPath)
			if err := t.TailPath(absPath); err != nil {
				glog.Info(err)
			}
		}
	}
	return nil
}

// PollLogStreams looks at the existing paths and checks if they're already
// complete, removing it from the map if so.
func (t *Tailer) PollLogStreams() error {
	t.logstreamsMu.Lock()
	defer t.logstreamsMu.Unlock()
	for name, l := range t.logstreams {
		if l.IsComplete() {
			glog.Infof("%s is complete", name)
			delete(t.logstreams, name)
			logCount.Add(-1)
			continue
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
	if err := t.PollLogStreams(); err != nil {
		return err
	}
	return nil
}
