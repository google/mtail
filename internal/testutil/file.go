package testutil

import (
	"io"
	"testing"

	"github.com/golang/glog"
)

func WriteString(tb testing.TB, f io.StringWriter, str string) int {
	tb.Helper()
	n, err := f.WriteString(str)
	FatalIfErr(tb, err)
	glog.Infof("Wrote %d bytes", n)
	return n
}
