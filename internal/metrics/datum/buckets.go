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

// Buckets describes a floating point value at a given timestamp.
type Buckets struct {
	BaseDatum
	sync.RWMutex
	Buckets []BucketCount
	Count   uint64
	Sum     float64
}

func (d *Buckets) ValueString() string {
	return fmt.Sprintf("%g", d.GetSum())
}

func (d *Buckets) Observe(v float64, ts time.Time) {
	d.Lock()
	defer d.Unlock()

	for i, b := range d.Buckets {
		if v <= b.Range.Max {
			d.Buckets[i].Count++
			break
		}
	}

	d.Count++
	d.Sum += v

	d.stamp(ts)
}

func (d *Buckets) GetCount() uint64 {
	d.RLock()
	defer d.RUnlock()
	return d.Count
}

func (d *Buckets) GetSum() float64 {
	d.RLock()
	defer d.RUnlock()

	return d.Sum
}

func (d *Buckets) AddBucket(r Range) {
	d.Lock()
	defer d.Unlock()

	d.Buckets = append(d.Buckets, BucketCount{r, 0})
}

func (d *Buckets) GetBuckets() map[Range]uint64 {
	d.RLock()
	defer d.RUnlock()

	b := make(map[Range]uint64)
	for _, bc := range d.Buckets {
		b[bc.Range] = bc.Count
	}
	return b
}

func (d *Buckets) MarshalJSON() ([]byte, error) {
	d.RLock()
	defer d.RUnlock()

	bs := make(map[string]uint64)

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

func (r *Range) MarshalJSON() ([]byte, error) {
	j := struct {
		Min string
		Max string
	}{fmt.Sprintf("%v", r.Min), fmt.Sprintf("%v", r.Max)}

	return json.Marshal(j)
}
