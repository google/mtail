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

type ReadDeadliner interface {
	SetReadDeadline(t time.Time) error
}

func SetReadDeadlineOnDone(ctx context.Context, d ReadDeadliner) {
	go func() {
		<-ctx.Done()
		glog.V(1).Info("cancelled, setting read deadline to interrupt read")
		if err := d.SetReadDeadline(time.Now()); err != nil {
			glog.V(1).Infof("SetReadDeadline() -> %v", err)
		}
	}()
}

func IsEndOrCancel(err error) bool {
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
