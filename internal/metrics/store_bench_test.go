// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"fmt"
	"math"
	"math/rand"
	"reflect"
	"testing"
	"testing/quick"
)

const maxItemsLog2 = 10

// newRandMetric makes a new, randomly filled Metric
func newRandMetric(tb testing.TB, rand *rand.Rand, i int) *Metric {
	nameVal, ok := quick.Value(reflect.TypeOf(""), rand)
	if !ok {
		tb.Fatalf("%d: can't make a name", i)
	}
	progVal, ok := quick.Value(reflect.TypeOf(""), rand)
	if !ok {
		tb.Fatalf("%d: can't make a prog", i)
	}
	kindVal, ok := quick.Value(reflect.TypeOf(Counter), rand)
	if !ok {
		tb.Fatalf("%d: can't make a kind", i)
	}
	typeVal, ok := quick.Value(reflect.TypeOf(Int), rand)
	if !ok {
		tb.Fatalf("%d: can't make a type", i)
	}
	keysVal, ok := quick.Value(reflect.TypeOf([]string{}), rand)
	if !ok {
		tb.Fatalf("%d: can't make a key list", i)
	}
	return NewMetric(nameVal.Interface().(string),
		progVal.Interface().(string),
		kindVal.Interface().(Kind),
		typeVal.Interface().(Type),
		keysVal.Interface().([]string)...)
}

type bench struct {
	name  string
	setup func(b *testing.B, rand *rand.Rand, items int, m *[]*Metric, s *Store)
	b     func(b *testing.B, items int, m []*Metric, s *Store)
}

func fillMetric(b *testing.B, rand *rand.Rand, items int, m *[]*Metric, _ *Store) {
	for i := 0; i < items; i++ {
		(*m)[i] = newRandMetric(b, rand, i)
	}
}

func addToStore(b *testing.B, items int, m []*Metric, s *Store) {
	for j := 0; j < items; j++ {
		s.Add(m[j])
	}
}

func BenchmarkStore(b *testing.B) {
	benches := []bench{
		{
			name:  "Add",
			setup: fillMetric,
			b:     addToStore,
		},
		{
			name: "Iterate",
			setup: func(b *testing.B, rand *rand.Rand, items int, m *[]*Metric, s *Store) {
				fillMetric(b, rand, items, m, s)
				addToStore(b, items, *m, s)
			},
			b: func(b *testing.B, items int, m []*Metric, s *Store) {
				s.SearchMu.RLock()
				for _ = range s.Metrics {
				}
				s.SearchMu.RUnlock()
			},
		},
	}
	rand := rand.New(rand.NewSource(99))
	for _, bench := range benches {
		for _, gc := range []bool{false, true} {
			gcStr := ""
			if gc {
				gcStr = "WithGc"
			}
			for _, parallel := range []bool{false, true} {
				parallelStr := ""
				if parallel {
					parallelStr = "Parallel"
				}

				for i := 0.; i <= maxItemsLog2; i++ {
					items := int(math.Pow(2, i))
					b.Run(fmt.Sprintf("%s%s%s-%d", bench.name, gcStr, parallelStr, items), func(b *testing.B) {
						s := NewStore()
						m := make([]*Metric, items)
						if bench.setup != nil {
							bench.setup(b, rand, items, &m, s)
						}
						b.ResetTimer()
						if parallel {
							b.RunParallel(func(pb *testing.PB) {
								for pb.Next() {
									bench.b(b, items, m, s)
								}
							})
						} else {
							for n := 0; n < b.N; n++ {
								bench.b(b, items, m, s)
								if gc {
									s.Gc()
								}
							}
						}
					})
				}
			}
		}
	}
}
