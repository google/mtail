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
	"bytes"
	"expvar"
	"fmt"
	"html/template"
	"io"
	"os"
	"path/filepath"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/golang/glog"
	"github.com/pkg/errors"

	"github.com/google/mtail/watcher"

	"github.com/spf13/afero"
)

var (
	LogCount     = expvar.NewInt("log_count")
	LogErrors    = expvar.NewMap("log_errors_total")
	LogRotations = expvar.NewMap("log_rotations_total")
)

// Tailer receives notification of changes from a Watcher and extracts new log
// lines from files. It also handles new log file creation events and log
// rotations.
type Tailer struct {
	w watcher.Watcher

	lines chan<- *LogLine // Logfile lines being emitted.

	watched        map[string]struct{}      // Names of logs being watched.
	watchedMu      sync.RWMutex             // protects `watched'
	files          map[string]afero.File    // File handles for each pathname.
	filesMu        sync.Mutex               // protects `files'
	partials       map[string]*bytes.Buffer // Accumulator for the currently read line for each pathname.
	partialsMu     sync.Mutex               // protects 'partials'
	globPatterns   map[string]struct{}      // glob patterns to match newly created files in dir paths against
	globPatternsMu sync.RWMutex             // protects `globPatterns'

	stopForever chan struct{} // Signals termination to the readForever goroutine
	runDone     chan struct{} // Signals termination of the run goroutine.

	fs afero.Fs // mockable filesystem interface

	oneShot bool
}

// Options configures a Tailer
type Options struct {
	Lines   chan<- *LogLine // output channel of lines read
	OneShot bool            // if true, reads from start and exits after each file hits eof
	W       watcher.Watcher
	FS      afero.Fs
}

// New returns a new Tailer, configured with the supplied Options
func New(o Options) (*Tailer, error) {
	if o.Lines == nil {
		return nil, errors.New("can't create tailer without lines channel")
	}
	if o.FS == nil {
		return nil, errors.New("can't create tailer without FS")
	}
	if o.W == nil {
		return nil, errors.New("can't create tailer without W")
	}
	t := &Tailer{
		w:            o.W,
		watched:      make(map[string]struct{}),
		lines:        o.Lines,
		files:        make(map[string]afero.File),
		partials:     make(map[string]*bytes.Buffer),
		globPatterns: make(map[string]struct{}),
		fs:           o.FS,
		oneShot:      o.OneShot,
		stopForever:  make(chan struct{}),
		runDone:      make(chan struct{}),
	}
	eventsChan := t.w.Events()
	go t.run(eventsChan)
	return t, nil
}

// addWatched adds a path to the list of watched items.
func (t *Tailer) addWatched(path string) error {
	absPath, err := filepath.Abs(path)
	if err != nil {
		return errors.Wrapf(err, "Failed to lookup absolutepath of %q", path)
	}
	glog.V(2).Infof("Adding a watch on resolved path %q", absPath)
	err = t.w.Add(absPath)
	if err != nil {
		if os.IsPermission(err) {
			glog.V(2).Infof("Skipping permission denied error on adding a watch.")
		} else {
			return errors.Wrapf(err, "Failed to create a new watch on %q", absPath)
		}
	}
	t.watchedMu.Lock()
	defer t.watchedMu.Unlock()
	t.watched[absPath] = struct{}{}
	return nil
}

// isWatching indicates if the path is being watched. It includes both
// filenames and directories.
func (t *Tailer) isWatching(path string) bool {
	absPath, err := filepath.Abs(path)
	if err != nil {
		glog.V(2).Infof("Couldn't resolve path %q: %s", absPath, err)
		return false
	}
	glog.V(2).Infof("Resolved path for lookup %q", absPath)
	t.watchedMu.RLock()
	defer t.watchedMu.RUnlock()
	_, ok := t.watched[absPath]
	return ok
}

// Tail registers a pattern to be tailed.  If pattern is a plain
// file then it is watched for updates and opened.  If pattern is a glob, then
// all paths that match the glob are opened and watched, and the directories
// containing those matches, if any, are watched.
func (t *Tailer) Tail(pattern string) error {
	matches, err := afero.Glob(t.fs, pattern)
	if err != nil {
		return err
	}
	t.globPatternsMu.Lock()
	t.globPatterns[pattern] = struct{}{}
	t.globPatternsMu.Unlock()
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
	if err := t.watchDirname(pattern); err != nil {
		return err
	}
	return nil
}

// TailPath registers a filesystem pathname to be tailed.
func (t *Tailer) TailPath(pathname string) error {
	if t.isWatching(pathname) {
		glog.V(2).Infof("already watching %q", pathname)
		return nil
	}
	if err := t.addWatched(pathname); err != nil {
		return err
	}
	LogCount.Add(1)
	// TODO(jaq): ex_test/filename.mtail requires we use the original pathname here, not fullpath
	return t.openLogPath(pathname, false)
}

// TailFile registers a file handle to be tailed.  There is no filesystem to
// watch, so no watches are registered, and no file paths are opened.
func (t *Tailer) TailFile(f afero.File) error {
	LogCount.Add(1)
	return t.startNewFile(f, false)
}

// handleLogUpdate reads all available bytes from an already opened file
// identified by pathname, and sends them to be processed on the lines channel.
func (t *Tailer) handleLogUpdate(pathname string) {
	glog.V(2).Infof("handleLogUpdate %s", pathname)
	t.filesMu.Lock()
	fd, ok := t.files[pathname]
	t.filesMu.Unlock()
	if !ok {
		glog.Warningf("No file descriptor found for %q, but is being watched; opening", pathname)
		// Try to open it, and because we have a watch set seenBefore.
		t.openLogPath(pathname, true)
		return
	}
	var err error
	t.partialsMu.Lock()
	err = t.read(fd, t.partials[pathname])
	t.partialsMu.Unlock()
	if err != nil && err != io.EOF {
		glog.Info(err)
	}
}

// handleTruncate checks to see if the current offset into the file
// is past the end of the file based on its size, and if so seeks to
// the start again.  Returns nil iff that happened.
func (t *Tailer) handleTruncate(f afero.File) error {
	currentOffset, err := f.Seek(0, io.SeekCurrent)
	glog.V(2).Infof("current seek position at %d", currentOffset)
	if err != nil {
		return err
	}

	fi, err := f.Stat()
	if err != nil {
		return err
	}

	glog.V(2).Infof("File size is %d", fi.Size())
	if currentOffset == 0 || fi.Size() >= currentOffset {
		return fmt.Errorf("no truncate appears to have occurred")
	}

	p, serr := f.Seek(0, io.SeekStart)
	glog.V(2).Infof("Truncated?  Seeked to %d: %v", p, serr)
	return serr
}

// read reads blocks of 4096 bytes from the File, sending lines to the
// channel as it encounters newlines.  If EOF is encountered, the partial line
// is returned to be concatenated with on the next call.
func (t *Tailer) read(f afero.File, partial *bytes.Buffer) error {
	b := make([]byte, 0, 4096)
	ntotal := 0 // bytes read in this invocation
	for {
		n, err := f.Read(b[:cap(b)])
		glog.V(2).Infof("Read: %v %v", n, err)
		ntotal += n
		b = b[:n]

		if err == io.EOF && ntotal == 0 {
			glog.V(2).Info("Suspected truncation.")
			// If there was nothing to be read, perhaps the file just got truncated.
			herr := t.handleTruncate(f)
			glog.V(2).Infof("handletrunc with error '%v'", herr)
			if herr == nil {
				// Try again: offset was greater than filesize and now we've seeked to start.
				continue
			}
		}

		if err != nil {
			return err
		}

		var (
			rune  rune
			width int
		)
		for i := 0; i < len(b) && i < n; i += width {
			rune, width = utf8.DecodeRune(b[i:])
			switch {
			case rune != '\n':
				partial.WriteRune(rune)
			default:
				// send off line for processing, blocks if not ready
				t.lines <- NewLogLine(f.Name(), partial.String())
				// reset accumulator
				partial.Reset()
			}
		}
	}
}

// handleLogCreate handles both new and rotated log files.
func (t *Tailer) handleLogCreate(pathname string) {
	glog.V(2).Infof("handleLogCreate %s", pathname)
	t.filesMu.Lock()
	fd, ok := t.files[pathname]
	t.filesMu.Unlock()

	if !ok {
		// Freshly opened log file, never seen before.
		t.openLogPath(pathname, false)
		return
	}

	s1, err := fd.Stat()
	if err != nil {
		glog.Infof("Stat failed on %q: %s", t.files[pathname].Name(), err)
		// We have a fd but it's invalid, handle as a rotation (delete/create)
		LogRotations.Add(pathname, 1)
		LogCount.Add(1)
		t.openLogPath(pathname, true)
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
	LogRotations.Add(pathname, 1)
	// flush the old log, pathname is still an index into t.files with the old inode.
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
		t.openLogPath(pathname, true)
	}()
}

func (t *Tailer) handleLogDelete(pathname string) {
	glog.V(2).Infof("handleLogDelete %s", pathname)
	t.filesMu.Lock()
	fd, ok := t.files[pathname]
	t.filesMu.Unlock()
	if !ok {
		glog.V(2).Infof("Delete without fd for %s", pathname)
		return
	}
	// flush the old log, as pathname is still an index into t.files with the old inode still open
	t.handleLogUpdate(pathname)
	if err := fd.Close(); err != nil {
		glog.Warning(err)
	}
	LogCount.Add(-1)
	// Explicitly leave the filedescriptor invalid to test for log rotation in handleLogCreate
}

// watchDirname adds the directory containing a path to be watched.
func (t *Tailer) watchDirname(pathname string) error {
	absPath, err := filepath.Abs(pathname)
	if err != nil {
		return err
	}
	d := filepath.Dir(absPath)
	if !t.isWatching(d) {
		return t.addWatched(d)
	}
	return nil
}

// openLogPath opens a log file named by pathname.
func (t *Tailer) openLogPath(pathname string, seenBefore bool) error {
	glog.V(2).Infof("openlogPath %s %v", pathname, seenBefore)
	if err := t.watchDirname(pathname); err != nil {
		return err
	}

	retries := 3
	retryDelay := 1 * time.Millisecond
	shouldRetry := func() bool {
		// seenBefore indicates also that we're rotating a file that previously worked, so retry.
		if !seenBefore {
			return false
		}
		return retries > 0
	}
	var f afero.File
	var err error
Retry:
	f, err = t.fs.Open(pathname)
	if err == nil {
		glog.V(2).Infof("open succeeded %s", pathname)
	}
	// Doesn't exist yet. We're watching the directory, so we'll pick it up
	// again on create; return successfully.
	if os.IsNotExist(err) {
		glog.V(1).Infof("Pathname %q doesn't exist (yet?)", pathname)
		return nil
	}
	LogErrors.Add(pathname, 1)
	if shouldRetry() {
		retries = retries - 1
		time.Sleep(retryDelay)
		retryDelay = retryDelay + retryDelay
		goto Retry
	}
	if err != nil {
		return errors.Wrap(err, "openLogPath failed all retries")
	}
	err = t.startNewFile(f, seenBefore)
	if err != nil && err != io.EOF {
		glog.Error(err)
		return err
	}
	return nil
}

// startNewFile optionally seeks to the start or end of the file f, then starts
// the consumption of log lines. Rotated logs and logs read in oneshot mode
// should read from the start, but logs opened for the first time read from the
// "current point in time", which is the end of the file for logs being
// appended to.
func (t *Tailer) startNewFile(f afero.File, seekStart bool) error {
	fi, err := f.Stat()
	if err != nil {
		// Stat failed, log error and return.
		LogErrors.Add(f.Name(), 1)
		return errors.Wrapf(err, "Failed to stat %q: %s", f.Name())
	}
	switch m := fi.Mode(); {
	case m.IsRegular():
		seekWhence := io.SeekEnd
		if seekStart || t.oneShot {
			seekWhence = io.SeekCurrent
		}
		if _, err := f.Seek(0, seekWhence); err != nil {
			return errors.Wrapf(err, "Seek failed on %q", f.Name())
		}
		glog.V(2).Infof("Adding a file watch on %q", f.Name())
		if err := t.addWatched(f.Name()); err != nil {
			return err
		}
		// In case the new log has been written to already, attempt to read the
		// first lines.
		t.partialsMu.Lock()
		t.partials[f.Name()] = bytes.NewBufferString("")
		err = t.read(f, t.partials[f.Name()])
		t.partialsMu.Unlock()
		if err != nil {
			if err == io.EOF {
				glog.V(1).Info("EOF on first read")
				if !t.oneShot {
					// Don't worry about EOF on first read, that's expected due to SEEK_END.
					break
				}
			}
			return err
		}
	case m&os.ModeType == os.ModeNamedPipe:
		t.partialsMu.Lock()
		t.partials[f.Name()] = bytes.NewBufferString("")
		t.partialsMu.Unlock()
		go t.readForever(f)
	default:
		return errors.Errorf("Can't open files with mode %v: %s", m&os.ModeType, f.Name())
	}
	t.filesMu.Lock()
	t.files[f.Name()] = f
	t.filesMu.Unlock()
	glog.Infof("Tailing %s", f.Name())

	return nil
}

// start is the main event loop for the Tailer.
// It receives notification of log file changes from the watcher channel, and
// handles them.
func (t *Tailer) run(events <-chan watcher.Event) {
	defer close(t.runDone)

	for e := range events {
		glog.V(2).Infof("Event type %#v", e)
		switch e := e.(type) {
		case watcher.UpdateEvent:
			if t.isWatching(e.Pathname) {
				t.handleLogUpdate(e.Pathname)
			}
		case watcher.CreateEvent:
			if t.isWatching(e.Pathname) {
				t.handleLogCreate(e.Pathname)
			} else {
				t.globPatternsMu.RLock()
				for pattern := range t.globPatterns {
					matched, err := filepath.Match(pattern, e.Pathname)
					if err != nil {
						glog.Warningf("Unexpected bad pattern %q not detected earlier", pattern)
						continue
					}
					if matched {
						glog.V(1).Infof("New file %q matched existing glob %q", e.Pathname, pattern)
						err := t.TailPath(e.Pathname)
						if err != nil {
							glog.Infof("Failed to tail new file %q: %s", e.Pathname, err)
						}
					}
				}
				t.globPatternsMu.RUnlock()
			}
		case watcher.DeleteEvent:
			if t.isWatching(e.Pathname) {
				t.handleLogDelete(e.Pathname)
			}
		default:
			glog.Infof("Unexpected event %#v", e)
		}
	}
	glog.Infof("Shutting down tailer.")
	close(t.lines)
}

// readForever handles non-logfile inputs by reading from the File until it is closed.
func (t *Tailer) readForever(f afero.File) {
	var err error
	partial := bytes.NewBufferString("")
	for {
		select {
		case <-t.stopForever:
			return
		default:
			err = t.read(f, partial)
			// We want to exit at EOF, because the FD has been closed.
			if err != nil {
				glog.Infof("error on partial read of %s (fd %v): %s", f.Name(), f, err)
				return
			}
			// TODO(jaq): nonblocking read, handle eagain, and do a little sleep if so, so the read can be interrupted
		}
	}
}

// Close signals termination to the watcher.
func (t *Tailer) Close() error {
	close(t.stopForever)
	if err := t.w.Close(); err != nil {
		return err
	}
	<-t.runDone
	return nil
}

const tailerTemplate = `
<h2 id="tailer">Log Tailer</h2>
{{range $name, $val := $.Watched}}
<p><b>{{$name}}</b></p>
{{end}}
`

func (t *Tailer) WriteStatusHTML(w io.Writer) error {
	tpl, err := template.New("tailer").Parse(tailerTemplate)
	if err != nil {
		return err
	}
	t.watchedMu.RLock()
	defer t.watchedMu.RUnlock()
	data := struct {
		Watched map[string]struct{}
	}{
		t.watched,
	}
	return tpl.Execute(w, data)
}
