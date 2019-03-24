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

type bucketCount struct {
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
	buckets []bucketCount
	count   uint64
	sum     float64
}

func (*BucketsDatum) Type() Type { return Buckets }

func (d *BucketsDatum) ValueString() string {
	return fmt.Sprintf("%g", d.Sum())
}

func (d *BucketsDatum) Observe(v float64, ts time.Time) {
	d.Lock()
	defer d.Unlock()

	for i, b := range d.buckets {
		if b.Range.Contains(v) {
			d.buckets[i].Count++
			break
		}
	}

	d.count++
	d.sum += v

	d.stamp(ts)
}

func (d *BucketsDatum) String() string {
	return fmt.Sprintf("%g@%d", d.Sum(), atomic.LoadInt64(&d.Time))
}

func (d *BucketsDatum) Count() uint64 {
	return atomic.LoadUint64(&d.count)
}

func (d *BucketsDatum) Sum() float64 {
	d.RLock()
	defer d.RUnlock()

	return d.sum
}

func (d *BucketsDatum) AddBucket(r Range) {
	d.Lock()
	defer d.Unlock()

	d.buckets = append(d.buckets, bucketCount{r, 0})
}

func (d *BucketsDatum) Buckets() map[Range]uint64 {
	d.RLock()
	defer d.RUnlock()

	b := make(map[Range]uint64)
	for _, bc := range d.buckets {
		b[bc.Range] = bc.Count
	}
	return b
}

func (d *BucketsDatum) MarshalJSON() ([]byte, error) {
	d.RLock()
	defer d.RUnlock()

	bs := make(map[string]uint64, 0)

	for _, b := range d.buckets {
		bs[strconv.FormatFloat(b.Range.Max, 'g', -1, 64)] = b.Count
	}

	j := struct {
		Buckets map[string]uint64
		Count   uint64
		Sum     float64
		Time    int64
	}{bs, d.count, d.sum, atomic.LoadInt64(&d.Time)}

	return json.Marshal(j)
}
