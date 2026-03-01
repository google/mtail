package logstream_test

import (
	"context"
	"os"
	"sync"
	"testing"

	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/waker"
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

func TestPathNameWithHash(t *testing.T) {
	ctx := context.Background()
	var wg sync.WaitGroup
	file, err := os.CreateTemp("", "4c9_1#1_2.3.4.a.b114514#1bcd.log")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(file.Name())
	defer file.Close()
	wk := waker.NewTestAlways()
	wk.Wake()
	_, err = logstream.New(ctx, &wg, wk, file.Name(), logstream.OneShotDisabled)
	if err != nil {
		t.Errorf("New(ctx, nil) expecting nil, received error: %v", err)
	}
	_, err = logstream.New(ctx, &wg, wk, "file://"+file.Name(), logstream.OneShotDisabled)
	if err != nil {
		t.Errorf("New(ctx, nil) expecting nil, received error: %v", err)
	}
}
