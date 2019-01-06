// +build integration

package mtail_test

import (
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"path"
	"strings"
	"testing"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/watcher"
	"github.com/spf13/afero"
)

func tempDir(t *testing.T) (string, func()) {
	t.Helper()
	name, err := ioutil.TempDir("", "mtail-test")
	if err != nil {
		t.Fatal(err)
	}
	return name, func() {
		if err := os.RemoveAll(name); err != nil {
			t.Fatalf("os.RemoveAll(%s): %s", name, err)
		}
	}
}

func MakeServer(t *testing.T, options ...func(*mtail.Server) error) (*mtail.Server, error) {
	t.Helper()
	w, err := watcher.NewLogWatcher(0, true)
	if err != nil {
		t.Fatal(err)
	}

	return mtail.New(metrics.NewStore(), w, &afero.OsFs{}, options...)
}

func StartServer(t *testing.T, options ...func(*mtail.Server) error) (*mtail.Server, func()) {
	t.Helper()
	l, err := net.Listen("tcp", ":0")
	if err != nil {
		t.Fatal(err)
	}
	ip := l.Addr().(*net.TCPAddr).IP
	port := l.Addr().(*net.TCPAddr).Port
	options = append(options, mtail.BindAddress(ip.String(), fmt.Sprintf("%d", port)))

	m, err := MakeServer(t, options...)
	if err != nil {
		t.Fatal(err)
	}

	return m, func() {
		m.Close()
	}
}

func TestBadProgramFailsCompilation(t *testing.T) {
	progDir, rmProgDir := tempDir(t)
	defer rmProgDir()
	logDir, rmLogDir := tempDir(t)
	defer rmLogDir()

	err := ioutil.WriteFile(path.Join(progDir, "bad.mtail"), []byte("asdfasdf\n"), 0666)
	if err != nil {
		t.Fatal(err)
	}

	// Compile-only fails program compilation at server start, not after it's running.
	_, err = MakeServer(t, mtail.ProgramPath(progDir), mtail.LogPathPatterns(logDir), mtail.CompileOnly)
	if err == nil {
		t.Error("expected error from mtail")
	}
	if !strings.Contains(err.Error(), "compile failed") {
		t.Error("compile failed not reported")
	}
}
