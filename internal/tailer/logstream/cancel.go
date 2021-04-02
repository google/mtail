package logstream

import (
	"context"
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
		glog.Info("cancelled, setting read deadline to interrupt read")
		d.SetReadDeadline(time.Now())
	}()
}

func IsEndOrCancel(err error) bool {
	if err == io.EOF {
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
