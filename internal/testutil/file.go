package testutil

import (
	"os"
	"testing"

	"github.com/golang/glog"
)

func WriteString(tb testing.TB, f *os.File, str string) int {
	tb.Helper()
	n, err := f.WriteString(str)
	FatalIfErr(tb, err)
	glog.Infof("Wrote %d bytes", n)
	return n
}
