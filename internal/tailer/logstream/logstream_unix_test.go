//go:build unix

package logstream

import (
	"context"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"github.com/google/mtail/internal/testutil"
	"golang.org/x/sys/unix"
)

func TestReadStdin(t *testing.T) {
	ctx := context.Background()
	var wg sync.WaitGroup
	// Need to pretend Stdin is a fifo for this to pass.
	oldStdin := os.Stdin
	t.Cleanup(func() { os.Stdin = oldStdin })

	tmpDir := testutil.TestTempDir(t)
	name := filepath.Join(tmpDir, "fakeStdin")
	testutil.FatalIfErr(t, unix.Mkfifo(name, 0o666))
	var err error
	os.Stdin, err = os.OpenFile(name, os.O_RDWR, os.ModeNamedPipe)
	testutil.FatalIfErr(t, err)

	_, err = New(ctx, &wg, nil, "-", nil, false)
	if err != nil {
		t.Errorf("New(.., '-') -> %v, expecting nil", err)
	}
}
