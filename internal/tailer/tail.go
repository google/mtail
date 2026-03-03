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
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"sync"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/waker"
)

// logCount records the number of logs that are being tailed.
var logCount = expvar.NewInt("log_count")

// Tailer polls the filesystem for log sources that match given
// `LogPathPatterns` and creates `LogStream`s to tail them.
type Tailer struct {
	ctx    context.Context
	cancel context.CancelFunc

	wg sync.WaitGroup // Wait for our subroutines to finish

	lines chan<- *logline.LogLine

	logPatterns []string

	logPatternPollWaker waker.Waker         // Used to poll for new logs
	globPatternsMu      sync.RWMutex        // protects `globPatterns'
	globPatterns        map[string]struct{} // glob patterns to match newly created logs in dir paths against
	ignoreRegexPattern  *regexp.Regexp

	oneShot logstream.OneShotMode

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
var OneShot = &niladicOption{func(t *Tailer) error { t.oneShot = logstream.OneShotEnabled; return nil }}

// LogPatterns sets the glob patterns to use to match pathnames.
type LogPatterns []string

func (opt LogPatterns) apply(t *Tailer) error {
	t.logPatterns = opt
	return nil
}

// IgnoreRegex sets the regular expression to use to filter away pathnames that match the LogPatterns glob.
type IgnoreRegex string

func (opt IgnoreRegex) apply(t *Tailer) error {
	return t.SetIgnorePattern(string(opt))
}

// LogPatternPollWaker triggers polls on the filesystem for new logs that match the log glob patterns.
func LogPatternPollWaker(w waker.Waker) Option {
	return &logPatternPollWaker{w}
}

type logPatternPollWaker struct {
	waker.Waker
}

func (opt logPatternPollWaker) apply(t *Tailer) error {
	t.logPatternPollWaker = opt.Waker
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

var (
	ErrNoLinesChannel = errors.New("Tailer needs a lines channel")
	ErrNeedsWaitgroup = errors.New("tailer needs a WaitGroup")
)

// New creates a new Tailer.
func New(ctx context.Context, wg *sync.WaitGroup, lines chan<- *logline.LogLine, options ...Option) (*Tailer, error) {
	if lines == nil {
		return nil, ErrNoLinesChannel
	}
	if wg == nil {
		return nil, ErrNeedsWaitgroup
	}
	t := &Tailer{
		lines:        lines,
		initDone:     make(chan struct{}),
		globPatterns: make(map[string]struct{}),
		logstreams:   make(map[string]logstream.LogStream),
	}
	t.ctx, t.cancel = context.WithCancel(ctx)
	defer close(t.initDone)
	if err := t.SetOption(options...); err != nil {
		return nil, err
	}
	// After processing options, we can add patterns.  We need to ensure any Wakers were provided.
	for _, p := range t.logPatterns {
		if err := t.AddPattern(p); err != nil {
			return nil, err
		}
	}

	// This goroutine cancels the Tailer if all of our dependent subroutines are done.
	// These are any live logstreams, and any log pattern pollers.
	wg.Add(1)
	go func() {
		defer wg.Done()
		<-t.initDone
		t.wg.Wait()
		t.cancel()
	}()

	// This goroutine awaits cancellation, then cleans up the tailer.
	wg.Add(1)
	go func() {
		defer wg.Done()
		<-t.initDone
		<-t.ctx.Done()
		t.wg.Wait()
		glog.V(1).InfoContextf(ctx, "tailer finished")
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

var ErrUnsupportedURLScheme = errors.New("unsupported URL scheme")

// AddPattern adds a pattern to the list of patterns to filter filenames against.
func (t *Tailer) AddPattern(pattern string) error {
	u, err := url.Parse(pattern)
	if err != nil {
		return err
	}

	path := pattern
	switch u.Scheme {
	default:
		glog.V(2).Infof("AddPattern(%v): %v in path pattern %q, treating as path", pattern, ErrUnsupportedURLScheme, u.Scheme)
		// Leave path alone per log message
	case "unix", "unixgram", "tcp", "udp", logstream.KafkaScheme, logstream.AWS3Scheme:
		// Keep the scheme.
		glog.V(2).Infof("AddPattern(%v): is a socket", path)
		return t.TailPath(path)
	case "", "file":
		// Leave path alone; may contain globs
	}
	if logstream.IsStdinPattern(pattern) {
		// stdin is not really a socket, but it is handled by this codepath and should not be in the globs.
		glog.V(2).Infof("AddPattern(%v): is stdin", pattern)
		return t.TailPath(pattern)
	}
	path, err = filepath.Abs(path)
	if err != nil {
		glog.V(2).Infof("AddPattern(%v): couldn't canonicalize path: %v", path, err)
		return err
	}
	glog.V(2).Infof("AddPattern(%v): is a file-like pattern", path)
	t.globPatternsMu.Lock()
	t.globPatterns[path] = struct{}{}
	t.globPatternsMu.Unlock()
	t.pollLogPattern(path)
	return nil
}

func (t *Tailer) Ignore(pathname string) bool {
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		glog.V(2).Infof("Ignore(%v): couldn't get absolute path: %v", pathname, err)
		return true
	}
	fi, err := os.Stat(absPath)
	if err != nil {
		glog.V(2).Infof("Ignore(%v): couldn't stat: %v", pathname, err)
		return true
	}
	if fi.Mode().IsDir() {
		glog.V(2).Infof("Ignore(%v): is a folder", pathname)
		return true
	}
	return t.ignoreRegexPattern != nil && t.ignoreRegexPattern.MatchString(fi.Name())
}

func (t *Tailer) SetIgnorePattern(pattern string) error {
	if len(pattern) == 0 {
		return nil
	}
	glog.V(2).Infof("Set filename ignore regex pattern %q", pattern)
	ignoreRegexPattern, err := regexp.Compile(pattern)
	if err != nil {
		glog.V(2).Infof("Couldn't compile regex %q: %s", pattern, err)
		fmt.Printf("error: %v\n", err)
		return err
	}
	t.ignoreRegexPattern = ignoreRegexPattern
	return nil
}

// TailPath registers a filesystem pathname to be tailed.
func (t *Tailer) TailPath(pathname string) error {
	t.logstreamsMu.Lock()
	defer t.logstreamsMu.Unlock()
	if _, ok := t.logstreams[pathname]; ok {
		glog.V(2).Infof("already got a logstream on %q", pathname)
		return nil
	}
	l, err := logstream.New(t.ctx, &t.wg, t.logstreamPollWaker, pathname, t.oneShot)
	if err != nil {
		return err
	}
	t.logstreams[pathname] = l
	t.wg.Add(1)
	// Start a goroutine to move lines from the logstream to the main Tailer
	// output and remove the stream from the map when the channel closes.
	go func() {
		defer t.wg.Done()
		for line := range l.Lines() {
			t.lines <- line
		}
		t.logstreamsMu.Lock()
		delete(t.logstreams, pathname)
		logCount.Add(-1)
		t.logstreamsMu.Unlock()
	}()
	glog.Infof("Tailing %s", pathname)
	logCount.Add(1)
	return nil
}

// pollLogPattern runs a permanent goroutine to poll for new log files that
// match `pattern`.  It is on the subroutine waitgroup as we do not want to
// shut down the tailer when there are outstanding patterns to poll for.
func (t *Tailer) pollLogPattern(pattern string) {
	if err := t.doPatternGlob(pattern); err != nil {
		glog.Infof("pollPattern(%v): glob failed: %v", pattern, err)
	}
	if t.logPatternPollWaker == nil {
		glog.Infof("pollPattern(%v): log pattern polling disabled by no waker", pattern)
		return
	}
	t.wg.Add(1)
	go func() {
		defer t.wg.Done()
		<-t.initDone
		if t.oneShot {
			glog.Infof("pollPattern(%v): no polling loop in oneshot mode", pattern)
			return
		}
		glog.V(1).Infof("pollPattern(%v): starting log pattern poll loop", pattern)
		for {
			select {
			case <-t.ctx.Done():
				return
			case <-t.logPatternPollWaker.Wake():
				if err := t.doPatternGlob(pattern); err != nil {
					glog.Infof("pollPattern(%v): glob failed: %v", pattern, err)
				}
			}
		}
	}()
}

// doPatternGlob matches a glob-style pattern against the filesystem and issues
// a TailPath for any files that match.
func (t *Tailer) doPatternGlob(pattern string) error {
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return err
	}
	glog.V(1).Infof("doPatternGlob(%v): glob matches: %v", pattern, matches)
	for _, pathname := range matches {
		if t.Ignore(pathname) {
			continue
		}
		absPath, err := filepath.Abs(pathname)
		if err != nil {
			glog.V(2).Infof("doPatternGlob(%v): couldn't get absolute path for %q: %s", pattern, pathname, err)
			continue
		}
		glog.V(2).Infof("doPatternGlob(%v): tailable path is %q", pattern, absPath)
		if err := t.TailPath(absPath); err != nil {
			glog.Info(err)
		}
	}
	return nil
}
