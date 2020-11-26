// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logstream_test

import (
	"context"
	"sync"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
)

type stubProcessor struct {
	wg       sync.WaitGroup
	resultMu sync.Mutex
	result   []logline.LogLine
}

func NewStubProcessor() *stubProcessor {
	return &stubProcessor{
		result: make([]logline.LogLine, 0),
	}
}

func (s *stubProcessor) ExpectLinesReceived(n int) {
	s.wg.Add(n)
}

func (s *stubProcessor) Verify() {
	s.wg.Wait()
}

func (s *stubProcessor) ProcessLogLine(ctx context.Context, ll *logline.LogLine) {
	glog.Info("Line received")
	s.resultMu.Lock()
	s.result = append(s.result, *ll)
	s.resultMu.Unlock()
	s.wg.Done()
}

func (s *stubProcessor) Result() (r []logline.LogLine) {
	s.resultMu.Lock()
	defer s.resultMu.Unlock()
	r = make([]logline.LogLine, len(s.result))
	copy(r, s.result)
	return
}
