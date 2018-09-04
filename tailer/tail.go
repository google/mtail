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
	"expvar"
	"html/template"
	"io"
	"os"
	"path/filepath"
	"sync"

	"github.com/golang/glog"
	"github.com/pkg/errors"

	"github.com/google/mtail/logline"
	"github.com/google/mtail/tailer/file"
	"github.com/google/mtail/watcher"

	"github.com/spf13/afero"
)

var (
	// logCount records the number of logs that are being tailed
	logCount = expvar.NewInt("log_count")
)

// Tailer receives notification of changes from a Watcher and extracts new log
// lines from files. It also handles new log file creation events and log
// rotations.
type Tailer struct {
	lines chan<- *logline.LogLine // Logfile lines being emitted.
	w     watcher.Watcher
	fs    afero.Fs // mockable filesystem interface

	handlesMu sync.RWMutex          // protects `handles'
	handles   map[string]*file.File // File handles for each pathname.

	globPatternsMu sync.RWMutex        // protects `globPatterns'
	globPatterns   map[string]struct{} // glob patterns to match newly created files in dir paths against

	runDone chan struct{} // Signals termination of the run goroutine.

	eventsHandle int // record the handle with which to add new log files to the watcher

	oneShot bool
}

// OneShot puts the tailer in one-shot mode.
func OneShot(t *Tailer) error {
	t.oneShot = true
	return nil
}

// New creates a new Tailer.
func New(lines chan<- *logline.LogLine, fs afero.Fs, w watcher.Watcher, options ...func(*Tailer) error) (*Tailer, error) {
	if lines == nil {
		return nil, errors.New("can't create tailer without lines channel")
	}
	if fs == nil {
		return nil, errors.New("can't create tailer without FS")
	}
	if w == nil {
		return nil, errors.New("can't create tailer without W")
	}
	t := &Tailer{
		lines:        lines,
		w:            w,
		fs:           fs,
		handles:      make(map[string]*file.File),
		globPatterns: make(map[string]struct{}),
		runDone:      make(chan struct{}),
	}
	if err := t.SetOption(options...); err != nil {
		return nil, err
	}
	handle, eventsChan := t.w.Events()
	t.eventsHandle = handle
	go t.run(eventsChan)
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
func (t *Tailer) setHandle(pathname string, f *file.File) error {
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
func (t *Tailer) handleForPath(pathname string) (*file.File, bool) {
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
func (t *Tailer) AddPattern(pattern string) {
	glog.V(2).Infof("AddPattern: %s", pattern)
	t.globPatternsMu.Lock()
	t.globPatterns[pattern] = struct{}{}
	t.globPatternsMu.Unlock()
}

// TailPattern registers a pattern to be tailed.  If pattern is a plain
// file then it is watched for updates and opened.  If pattern is a glob, then
// all paths that match the glob are opened and watched, and the directories
// containing those matches, if any, are watched.
func (t *Tailer) TailPattern(pattern string) error {
	matches, err := afero.Glob(t.fs, pattern)
	if err != nil {
		return err
	}
	t.AddPattern(pattern)
	glog.V(1).Infof("glob matches: %v", matches)
	// TODO(jaq): Error if there are no matches, or do we just assume that it's OK?
	// mtail_test.go assumes that it's ok.  Figure out why.
	// if len(matches) == 0 {
	// 	return errors.Errorf("No matches for pattern %q", pattern)
	// }
	for _, pathname := range matches {
		err := t.TailPath(pathname)
		if err != nil {
			return errors.Wrapf(err, "attempting to tail %q", pathname)
		}
	}
	// Add a watch on the containing directory, so we know when a rotation
	// occurs or something shows up that matches this pattern.  TODO(jaq): this
	// seems fallible.
	return t.watchDirname(pattern)
}

// TailPath registers a filesystem pathname to be tailed.
func (t *Tailer) TailPath(pathname string) error {
	if t.hasHandle(pathname) {
		glog.V(2).Infof("already watching %q", pathname)
		return nil
	}
	if err := t.w.Add(pathname, t.eventsHandle); err != nil {
		return err
	}
	logCount.Add(1)
	// TODO(jaq): ex_test/filename.mtail requires we use the original pathname here, not fullpath
	return t.openLogPath(pathname, false, false)
}

// handleLogUpdate is dispatched when an UpdateEvent is received, causing the
// tailer to read all available bytes from an already-opened file and send each
// log line onto lines channel.
func (t *Tailer) handleLogUpdate(pathname string) {
	glog.V(2).Infof("handleLogUpdate %s", pathname)
	fd, ok := t.handleForPath(pathname)
	if !ok {
		glog.V(1).Infof("No file handle found for %q, but is being watched", pathname)
		// We want to open files we have watches on in case the file was
		// unreadable before now; but we have to copmare against the glob to be
		// sure we don't just add all the files in a watched directory as they
		// get modified.
		t.handleCreateGlob(pathname)
		return
	}
	// absPath, err := filepath.Abs(pathname)
	// if err != nil {
	// 	glog.Info(err)
	// }
	err := fd.Read(t.lines)
	if err != nil && err != io.EOF {
		// If the error is a patherror, and is because the file is closed, then
		// we're here because the file was rotated but we saw the CREATE before
		// the REMOVE.
		glog.Infof("%#V", err)
		if isFileClosedError(err) {
			t.handleCreateGlob(pathname)
			return
		}
	}
}

// handleLogCreate handles both new and rotated log files.
func (t *Tailer) handleLogCreate(pathname string) {
	glog.V(2).Infof("handleLogCreate %s", pathname)
	fd, ok := t.handleForPath(pathname)
	if !ok {
		t.handleCreateGlob(pathname)
		return
	}

	glog.V(2).Infof("c handleforPath %s is %s", pathname, fd.Name())
	if pathname != fd.Name() {
		glog.V(1).Infof("created name %s doesn't match name in handle %s", pathname, fd.Name())
		t.handleLogDelete(pathname)
		glog.V(2).Info("finished delete")
		t.handleCreateGlob(pathname)
		return
	}

	s1, err := fd.Stat()
	if err != nil {
		glog.Infof("Stat failed on %q: %s", fd.Pathname, err)
		// We have a fd but it's invalid, handle as a rotation (delete/create)
		// logRotations.Add(pathname, 1)
		// logCount.Add(1)
		// TODO(jaq): openlogpath seenBefore is true, so retry.
		if oerr := t.openLogPath(pathname, true, true); oerr != nil {
			glog.Warning(oerr)
		}
		return
	}
	s2, err := t.fs.Stat(pathname)
	if err != nil {
		glog.Infof("Stat failed on %q: %s", pathname, err)
		return
	}
	if os.SameFile(s1, s2) {
		glog.V(1).Infof("Path %s already being watched, and inode not changed.",
			pathname)
		return

	}
	glog.V(1).Infof("New inode detected for %s, treating as rotation.", pathname)
	//logRotations.Add(pathname, 1)
	// flush the old log, pathname is still an index into t.handles with the old inode.
	t.handleLogUpdate(pathname)
	if err := fd.Close(); err != nil {
		glog.Info(err)
	}
	go func() {
		// Run in goroutine as Remove may block waiting on event processing.
		if err := t.w.Remove(pathname); err != nil {
			glog.Infof("Failed removing watches on %s: %s", pathname, err)
		}
		// openLogPath readds the file to the watcher, so must be strictly after the Remove succeeds.
		// seenBefore is true, so retry
		if err := t.openLogPath(pathname, true, true); err != nil {
			glog.Warning(err)
		}
	}()
}

func (t *Tailer) handleLogDelete(pathname string) {
	glog.V(2).Infof("handleLogDelete %s", pathname)
	fd, ok := t.handleForPath(pathname)
	if !ok {
		glog.V(2).Infof("Delete without fd for %s", pathname)
		return
	}
	// flush the old log, as pathname is still an index into t.handles with the old inode still open
	t.handleLogUpdate(pathname)
	if err := fd.Close(); err != nil {
		glog.Warning(err)
	}
	logCount.Add(-1)
	// Explicitly leave the filehandle invalid to test for log rotation in handleLogCreate
}

// watchDirname adds the directory containing a path to be watched.
func (t *Tailer) watchDirname(pathname string) error {
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		return err
	}
	d := filepath.Dir(absPath)
	return t.w.Add(d, t.eventsHandle)
}

// openLogPath opens a log file named by pathname.
// TODO(jaq): seenBefore is incorrect for all log creation events received via fsnotify.
func (t *Tailer) openLogPath(pathname string, seenBefore, seekToStart bool) error {
	glog.V(2).Infof("openlogPath %s %v %v", pathname, seenBefore, seekToStart)
	if err := t.watchDirname(pathname); err != nil {
		return err
	}
	f, err := file.New(t.fs, pathname, seenBefore, seekToStart || t.oneShot)
	if err != nil {
		// Doesn't exist yet. We're watching the directory, so we'll pick it up
		// again on create; return successfully.
		if os.IsNotExist(err) {
			glog.V(1).Infof("AbsPath %q doesn't exist (yet?)", f.Pathname)
			return nil
		}
		return err
	}
	// The file did not exist when
	if f == nil {
		return nil
	}
	glog.V(2).Infof("Adding a file watch on %q", f.Pathname)
	if err := t.w.Add(f.Pathname, t.eventsHandle); err != nil {
		return err
	}
	if err := t.setHandle(pathname, f); err != nil {
		return err
	}
	if err := f.Read(t.lines); err != nil {
		if err == io.EOF {
			glog.V(1).Info("EOF on first read")
			// Don't worry about EOF on first read, that's expected due to SEEK_END.
			if t.oneShot {
				return err
			}
		}
	}
	glog.Infof("Tailing %s", f.Pathname)
	return nil
}

// handleCreateGlob matches the pathname against the glob patterns and starts tailing the file.
func (t *Tailer) handleCreateGlob(pathname string) {
	t.globPatternsMu.RLock()
	defer t.globPatternsMu.RUnlock()

	for pattern := range t.globPatterns {
		matched, err := filepath.Match(pattern, pathname)
		if err != nil {
			glog.Warningf("Unexpected bad pattern %q not detected earlier", pattern)
			continue
		}
		if !matched {
			glog.V(2).Infof("No match for %q", pathname)
			continue
		}
		glog.V(1).Infof("New file %q matched existing glob %q", pathname, pattern)
		// TODO(jaq): avoid code duplication with TailPath here.
		// If this file was just created, read from the start.
		if err := t.w.Add(pathname, t.eventsHandle); err != nil {
			glog.Infof("Failed to tail new file %q: %s", pathname, err)
			continue
		}
		logCount.Add(1)
		// Pretend seenBefore because we want to seek to start.
		if err := t.openLogPath(pathname, false, true); err != nil {
			glog.Infof("Failed to tail new file %q: %s", pathname, err)
		}
		glog.V(2).Infof("Started tailing %q", pathname)
	}
}

// start is the main event loop for the Tailer.
// It receives notification of log file changes from the watcher channel, and
// handles them.
func (t *Tailer) run(events <-chan watcher.Event) {
	defer close(t.runDone)

	for e := range events {
		glog.V(2).Infof("Event type %#v", e)
		switch e.Op {
		case watcher.Update:
			t.handleLogUpdate(e.Pathname)
		case watcher.Create:
			t.handleLogCreate(e.Pathname)
		case watcher.Delete:
			//t.handleLogDelete(e.Pathname)
		default:
			glog.Infof("Unexpected event %#v", e)
		}
	}
	glog.Infof("Shutting down tailer.")
	close(t.lines)
}

// Close signals termination to the watcher.
func (t *Tailer) Close() error {
	if err := t.w.Close(); err != nil {
		return err
	}
	<-t.runDone
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
		Handles   map[string]*file.File
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
	for name := range t.handles {
		if v := expvar.Get("log_errors_total").(*expvar.Map).Get(name); v != nil {
			data.Errors[name] = v.String()
		}
		if v := expvar.Get("log_rotations_total").(*expvar.Map).Get(name); v != nil {
			data.Rotations[name] = v.String()
		}
		if v := expvar.Get("log_lines_total").(*expvar.Map).Get(name); v != nil {
			data.Errors[name] = v.String()
		}
		if v := expvar.Get("log_truncates_total").(*expvar.Map).Get(name); v != nil {
			data.Errors[name] = v.String()
		}
	}
	return tpl.Execute(w, data)
}
