// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"context"
	"sync"

	"github.com/google/mtail/internal/logline"
)

type stubProcessor struct {
	result []*logline.LogLine
	wg     sync.WaitGroup
}

func NewStubProcessor() *stubProcessor {
	return &stubProcessor{
		result: make([]*logline.LogLine, 0),
	}
}

func (s *stubProcessor) Add(c int) {
	s.wg.Add(c)
}

func (s *stubProcessor) Wait() {
	s.wg.Wait()
}

func (s *stubProcessor) ProcessLogLine(ctx context.Context, ll *logline.LogLine) {
	s.result = append(s.result, ll)
	s.wg.Done()
}
