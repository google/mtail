// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"testing"
	"time"
)

func BenchmarkIncrementScalarInt(b *testing.B) {
	d := &intDatum{}
	ts := time.Now().UTC()
	for i := 0; i < b.N; i++ {
		d.IncBy(1, ts)
	}
}
