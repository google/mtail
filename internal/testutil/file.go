package testutil

import (
	"io"
	"os"
	"testing"

	"github.com/golang/glog"
)

func WriteString(tb testing.TB, f io.StringWriter, str string) int {
	tb.Helper()
	n, err := f.WriteString(str)
	FatalIfErr(tb, err)
	glog.Infof("Wrote %d bytes", n)
	// If this is a regular file (not a pipe or other StringWriter) then ensure
	// it's flushed to disk, to guarantee the write happens-before this
	// returns.
	if v, ok := f.(*os.File); ok {
		fi, err := v.Stat()
		FatalIfErr(tb, err)
		if fi.Mode().IsRegular() {
			glog.Infof("This is a regular file, doing a sync.")
			FatalIfErr(tb, v.Sync())
		}
	}
	return n
}
