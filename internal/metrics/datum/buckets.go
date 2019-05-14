// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"fmt"
	"strconv"
	"sync"
	"sync/atomic"
	"time"
)

type Range struct {
	Min float64
	Max float64
}

type BucketCount struct {
	Range Range
	Count uint64
}

func (r *Range) Contains(v float64) bool {
	return r.Min < v && v <= r.Max
}

// BucketsDatum describes a floating point value at a given timestamp.
type BucketsDatum struct {
	BaseDatum
	sync.RWMutex
	Buckets []BucketCount
	Count   uint64
	Sum     float64
}

func (*BucketsDatum) Type() Type { return Buckets }

func (d *BucketsDatum) ValueString() string {
	return fmt.Sprintf("%g", d.GetSum())
}

func (d *BucketsDatum) Observe(v float64, ts time.Time) {
	d.Lock()
	defer d.Unlock()

	for i, b := range d.Buckets {
		if b.Range.Contains(v) {
			d.Buckets[i].Count++
			break
		}
	}

	d.Count++
	d.Sum += v

	d.stamp(ts)
}

func (d *BucketsDatum) String() string {
	return fmt.Sprintf("%g@%d", d.GetSum(), atomic.LoadInt64(&d.Time))
}

func (d *BucketsDatum) GetCount() uint64 {
	return atomic.LoadUint64(&d.Count)
}

func (d *BucketsDatum) GetSum() float64 {
	d.RLock()
	defer d.RUnlock()

	return d.Sum
}

func (d *BucketsDatum) AddBucket(r Range) {
	d.Lock()
	defer d.Unlock()

	d.Buckets = append(d.Buckets, BucketCount{r, 0})
}

func (d *BucketsDatum) GetBuckets() map[Range]uint64 {
	d.RLock()
	defer d.RUnlock()

	b := make(map[Range]uint64)
	for _, bc := range d.Buckets {
		b[bc.Range] = bc.Count
	}
	return b
}

func (d *BucketsDatum) MarshalJSON() ([]byte, error) {
	d.RLock()
	defer d.RUnlock()

	bs := make(map[string]uint64, 0)

	for _, b := range d.Buckets {
		bs[strconv.FormatFloat(b.Range.Max, 'g', -1, 64)] = b.Count
	}

	j := struct {
		Buckets map[string]uint64
		Count   uint64
		Sum     float64
		Time    int64
	}{bs, d.Count, d.Sum, atomic.LoadInt64(&d.Time)}

	return json.Marshal(j)
}
