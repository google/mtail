package logstream

import (
	"context"
	"sync"
	"testing"
)

func TestNewErrors(t *testing.T) {
	ctx := context.Background()
	_, err := New(ctx, nil, nil, "", nil, false)
	if err == nil {
		t.Errorf("New(ctx, nil) expecting error, received nil")
	}
	var wg sync.WaitGroup
	_, err = New(ctx, &wg, nil, ":a/b", nil, false)
	if err == nil {
		t.Error("New(ctg, wg, ..., path) expecting error, received nil")
	}
}
