// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package tailer provides a class that is responsible for tailing log files
// and extracting new log lines to be passed into the virtual machines.
package tailer

// For regular files, mtail gets notified on modifications (i.e. appends) to
// log files that are being watched, in order to read the new lines. Log files
// can also be rotated, so mtail is also notified of creates in the log file
// directory.

import (
	"context"
	"expvar"
	"fmt"
	"html/template"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/pkg/errors"
	"go.opencensus.io/trace"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/watcher"
)

var (
	// logCount records the number of logs that are being tailed
	logCount = expvar.NewInt("log_count")
)

// Tailer receives notification of changes from a Watcher and extracts new log
// lines from files. It also handles new log file creation events and log
// rotations.
type Tailer struct {
	w   watcher.Watcher
	ctx context.Context
	llp logline.Processor

	handlesMu sync.RWMutex   // protects `handles'
	handles   map[string]Log // Log handles for each pathname.

	globPatternsMu     sync.RWMutex        // protects `globPatterns'
	globPatterns       map[string]struct{} // glob patterns to match newly created logs in dir paths against
	ignoreRegexPattern *regexp.Regexp

	oneShot bool
}

// OneShot puts the tailer in one-shot mode.
func OneShot(t *Tailer) error {
	t.oneShot = true
	return nil
}

// Context sets the context of the tailer
func Context(ctx context.Context) func(*Tailer) error {
	return func(t *Tailer) error {
		t.ctx = ctx
		return nil
	}
}

// New creates a new Tailer.
func New(llp logline.Processor, w watcher.Watcher, options ...func(*Tailer) error) (*Tailer, error) {
	if w == nil {
		return nil, errors.New("can't create tailer without W")
	}
	t := &Tailer{
		llp:          llp,
		w:            w,
		handles:      make(map[string]Log),
		globPatterns: make(map[string]struct{}),
	}
	if err := t.SetOption(options...); err != nil {
		return nil, err
	}
	return t, nil
}

// SetOption takes one or more option functions and applies them in order to Tailer.
func (t *Tailer) SetOption(options ...func(*Tailer) error) error {
	for _, option := range options {
		if err := option(t); err != nil {
			return err
		}
	}
	return nil
}

// setHandle sets a file handle under it's pathname
func (t *Tailer) setHandle(pathname string, f Log) error {
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		return errors.Wrapf(err, "Failed to lookup abspath of %q", pathname)
	}
	t.handlesMu.Lock()
	defer t.handlesMu.Unlock()
	t.handles[absPath] = f
	return nil
}

// handleForPath retrives a file handle for a pathname.
func (t *Tailer) handleForPath(pathname string) (Log, bool) {
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		glog.V(2).Infof("Couldn't resolve path %q: %s", pathname, err)
		return nil, false
	}
	t.handlesMu.Lock()
	defer t.handlesMu.Unlock()
	fd, ok := t.handles[absPath]
	return fd, ok
}

func (t *Tailer) hasHandle(pathname string) bool {
	_, ok := t.handleForPath(pathname)
	return ok
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

// TailPattern registers a pattern to be tailed.  If pattern is a plain
// file then it is watched for updates and opened.  If pattern is a glob, then
// all paths that match the glob are opened and watched, and the directories
// containing those matches, if any, are watched.
func (t *Tailer) TailPattern(pattern string) error {
	if err := t.AddPattern(pattern); err != nil {
		return err
	}
	// Add a watch on the containing directory, so we know when a rotation
	// occurs or something shows up that matches this pattern.
	if err := t.watchDirname(pattern); err != nil {
		return err
	}
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return err
	}
	glog.V(1).Infof("glob matches: %v", matches)
	// Error if there are no matches, but if they show up later, they'll get picked up by the directory watch set above.
	if len(matches) == 0 {
		return errors.Errorf("No matches for pattern %q", pattern)
	}
	for _, pathname := range matches {
		ignore, err := t.Ignore(pathname)
		if err != nil {
			return err
		}
		if ignore {
			continue
		}
		err = t.TailPath(pathname)
		if err != nil {
			return errors.Wrapf(err, "attempting to tail %q", pathname)
		}
	}
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
		// do directory stuff
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
	if t.hasHandle(pathname) {
		glog.V(2).Infof("already watching %q", pathname)
		return nil
	}
	if err := t.w.Observe(pathname, t); err != nil {
		return err
	}
	// New file at start of program, seek to EOF.
	return t.openLogPath(pathname, false)
}

// ProcessFileEvent is dispatched when an Event is received, causing the tailer
// to read all available bytes from an already-opened file and send each log
// line to the logline.Processor.  Because we handle rotations and truncates when
// reaching EOF in the file reader itself, we don't care what the signal is
// from the filewatcher.
func (t *Tailer) ProcessFileEvent(ctx context.Context, event watcher.Event) {
	ctx, span := trace.StartSpan(ctx, "Tailer.ProcessFileEvent")
	defer span.End()
	fd, ok := t.handleForPath(event.Pathname)
	if !ok {
		glog.V(1).Infof("No file handle found for %q, but is being watched", event.Pathname)
		// We want to open files we have watches on in case the file was
		// unreadable before now; but we have to copmare against the glob to be
		// sure we don't just add all the files in a watched directory as they
		// get modified.
		t.handleCreateGlob(ctx, event.Pathname)
		fd, ok = t.handleForPath(event.Pathname)
		if !ok {
			// This usually happens when a non-watched file in the same directory as a watched file gets updated.
			// TODO(jaq): add a unit test for this.
			glog.V(2).Infof("Internal error finding file handle for %q after create", event.Pathname)
			return
		}
	}
	doFollow(ctx, fd)
}

// doFollow performs the Follow on an existing file descriptor, logging any errors
func doFollow(ctx context.Context, fd Log) {
	err := fd.Follow(ctx)
	if err != nil && err != io.EOF {
		glog.Info(err)
	}
}

// watchDirname adds the directory containing a path to be watched.
func (t *Tailer) watchDirname(pathname string) error {
	glog.V(3).Infof("watchDirname: %s", pathname)
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		return err
	}
	d := filepath.Dir(absPath)
	for ; t.HasMeta(d); d = filepath.Dir(d) {
	}
	if d == "/" {
		glog.Infof("at root after recursing, won't observe %s", absPath)
		return nil
	}
	return t.w.Observe(d, t)
}

func (t *Tailer) HasMeta(path string) bool {
	magicChars := `*?[`
	if runtime.GOOS != "windows" {
		magicChars = `*?[\`
	}
	return strings.ContainsAny(path, magicChars)
}

// openLogPath opens a log file named by pathname.
func (t *Tailer) openLogPath(pathname string, seekToStart bool) error {
	glog.V(2).Infof("openlogPath %s %v", pathname, seekToStart)
	if err := t.watchDirname(pathname); err != nil {
		return err
	}
	f, err := NewLog(pathname, t.llp, seekToStart || t.oneShot)
	if err != nil {
		// Doesn't exist yet. We're watching the directory, so we'll pick it up
		// again on create; return successfully.
		if os.IsNotExist(err) {
			glog.V(1).Infof("pathname %q doesn't exist (yet?)", pathname)
			return nil
		}
		return err
	}
	glog.V(2).Infof("Adding a file watch on %q", f.Pathname())
	if err := t.w.Observe(f.Pathname(), t); err != nil {
		return err
	}
	if err := t.setHandle(pathname, f); err != nil {
		return err
	}
	// This is here for testing support mostly -- we don't want to read the
	// file before we've finished bootstrap because, for example, named pipes
	// don't have EOFs and files that update continuously can block Read from
	// termination.
	if t.oneShot {
		glog.V(2).Infof("Starting oneshot read at startup of %q", f.Pathname())
		if err := f.Read(t.ctx); err != nil && err != io.EOF {
			return err
		}
	}
	glog.Infof("Tailing %s", f.Pathname())
	logCount.Add(1)
	return nil
}

// handleCreateGlob matches the pathname against the glob patterns and starts tailing the file.
func (t *Tailer) handleCreateGlob(ctx context.Context, pathname string) {
	ctx, span := trace.StartSpan(ctx, "handleCreateGlob")
	defer span.End()
	t.globPatternsMu.RLock()
	defer t.globPatternsMu.RUnlock()

	for pattern := range t.globPatterns {
		matched, err := filepath.Match(pattern, pathname)
		if err != nil {
			glog.Warningf("Unexpected bad pattern %q not detected earlier", pattern)
			continue
		}
		if !matched {
			glog.V(2).Infof("%q did not match pattern %q", pathname, pattern)
			continue
		}
		ignore, err := t.Ignore(pathname)
		if err != nil {
			glog.Warningf("Unexpected bad pathname %q", pathname)
			continue
		}
		if ignore {
			glog.V(2).Infof("%q is ignored", pathname)
			continue
		}
		glog.V(1).Infof("New file %q matched existing glob %q", pathname, pattern)
		// If this file was just created, read from the start of the file.
		if err := t.openLogPath(pathname, true); err != nil {
			glog.Infof("Failed to tail new file %q: %s", pathname, err)
		}
		glog.V(2).Infof("started tailing %q", pathname)
		return
	}
	glog.V(2).Infof("did not start tailing %q", pathname)
}

// Close signals termination to the watcher.
func (t *Tailer) Close() error {
	if err := t.w.Close(); err != nil {
		return err
	}
	return nil
}

const tailerTemplate = `
<h2 id="tailer">Log Tailer</h2>
<h3>Patterns</h3>
<ul>
{{range $name, $val := $.Patterns}}
<li><pre>{{$name}}</pre></li>
{{end}}
</ul>
<h3>Log files watched</h3>
<table border=1>
<tr>
<th>pathname</th>
<th>errors</th>
<th>rotations</th>
<th>truncations</th>
<th>lines read</th>
</tr>
{{range $name, $val := $.Handles}}
<tr>
<td><pre>{{$name}}</pre></td>
<td>{{index $.Errors $name}}</td>
<td>{{index $.Rotations $name}}</td>
<td>{{index $.Truncs $name}}</td>
<td>{{index $.Lines $name}}</td>
</tr>
{{end}}
</table>
</ul>
`

// WriteStatusHTML emits the Tailer's state in HTML format to the io.Writer w.
func (t *Tailer) WriteStatusHTML(w io.Writer) error {
	tpl, err := template.New("tailer").Parse(tailerTemplate)
	if err != nil {
		return err
	}
	t.handlesMu.RLock()
	defer t.handlesMu.RUnlock()
	t.globPatternsMu.RLock()
	defer t.globPatternsMu.RUnlock()
	data := struct {
		Handles   map[string]Log
		Patterns  map[string]struct{}
		Rotations map[string]string
		Lines     map[string]string
		Errors    map[string]string
		Truncs    map[string]string
	}{
		t.handles,
		t.globPatterns,
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
	}
	for _, pair := range []struct {
		v *expvar.Map
		m map[string]string
	}{
		{logErrors, data.Errors},
		{logRotations, data.Rotations},
		{logTruncs, data.Truncs},
		{lineCount, data.Lines},
	} {
		pair.v.Do(func(kv expvar.KeyValue) {
			pair.m[kv.Key] = kv.Value.String()
		})
	}
	return tpl.Execute(w, data)
}

// Gc removes file handles that have had no reads for 24h or more.
func (t *Tailer) Gc() error {
	t.handlesMu.Lock()
	defer t.handlesMu.Unlock()
	for k, v := range t.handles {
		if time.Since(v.LastReadTime()) > (time.Hour * 24) {
			if err := t.w.Unobserve(v.Pathname(), t); err != nil {
				glog.Info(err)
			}
			if err := v.Close(t.ctx); err != nil {
				glog.Info(err)
			}
			delete(t.handles, k)
		}
	}
	return nil
}

// StartExpiryLoop runs a permanent goroutine to expire metrics every duration.
func (t *Tailer) StartGcLoop(duration time.Duration) {
	if duration <= 0 {
		glog.Info("Log handle expiration disabled")
		return
	}
	go func() {
		glog.Infof("Starting log handle expiry loop every %s", duration.String())
		ticker := time.NewTicker(duration)
		for range ticker.C {
			if err := t.Gc(); err != nil {
				glog.Info(err)
			}
		}
	}()
}
