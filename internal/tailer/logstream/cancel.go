package logstream

import (
	"context"
	"errors"
	"io"
	"os"
	"strings"
	"time"

	"github.com/golang/glog"
)

// ReadDeadliner has a SetReadDeadline function to be used for interrupting reads.
type ReadDeadliner interface {
	SetReadDeadline(t time.Time) error
}

// SetReadDeadlineOnDone waits for the context to be done, and then sets an
// immediate read deadline on the flie descriptor `d`.  This causes any blocked
// reads on that descriptor to return with an i/o timeout error.
func SetReadDeadlineOnDone(ctx context.Context, d ReadDeadliner) {
	go func() {
		<-ctx.Done()
		glog.V(1).Info("cancelled, setting read deadline to interrupt read")
		if err := d.SetReadDeadline(time.Now()); err != nil {
			glog.V(1).Infof("SetReadDeadline() -> %v", err)
		}
	}()
}

// IsExitableError returns true if a stream should exit because of this error.
func IsExitableError(err error) bool {
	if err == nil {
		return false
	}
	if errors.Is(err, io.EOF) {
		return true
	}
	if errors.Is(err, os.ErrClosed) {
		return true
	}
	if os.IsTimeout(err) {
		return true
	}
	// https://github.com/golang/go/issues/4373
	if strings.Contains(err.Error(), "use of closed network connection") {
		return true
	}
	return false
}
