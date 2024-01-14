package logstream

import (
	"context"
	"testing"
)

func TestNewErrors(t *testing.T) {
	ctx := context.Background()
	_, err := New(ctx, nil, nil, "", nil, false)
	if err == nil {
		t.Errorf("New(ctx, nil) expecting error, received nil")
	}
}
