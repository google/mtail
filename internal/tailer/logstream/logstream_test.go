package logstream_test

import (
	"context"
	"sync"
	"testing"

	"github.com/google/mtail/internal/tailer/logstream"
)

func TestNewErrors(t *testing.T) {
	ctx := context.Background()
	_, err := logstream.New(ctx, nil, nil, "", logstream.OneShotDisabled)
	if err == nil {
		t.Errorf("New(ctx, nil) expecting error, received nil")
	}
	var wg sync.WaitGroup
	_, err = logstream.New(ctx, &wg, nil, ":a/b", logstream.OneShotDisabled)
	if err == nil {
		t.Error("New(ctg, wg, ..., path) expecting error, received nil")
	}
}
