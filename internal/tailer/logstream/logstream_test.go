// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"sync"

	"github.com/google/mtail/internal/logline"
)

type StubProcessor struct {
	sync.WaitGroup
	Result []*logline.LogLine
}

func NewStubProcessor() *StubProcessor {
	return &StubProcessor{
		Result: make([]*logline.LogLine, 0),
	}
}

func (s *StubProcessor) ProcessLogLine(ctx context.Context, ll *logline.LogLine) {
	s.Result = append(s.Result, ll)
	s.WaitGroup.Done()
}
