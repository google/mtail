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

const (
	maxItemsLog2  = 10
	maxLabelsLog2 = 13
)

// newRandMetric makes a new, randomly filled Metric.
func newRandMetric(tb testing.TB, rand *rand.Rand, i int) *Metric {
	tb.Helper()
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
	b.Helper()
	for i := 0; i < items; i++ {
		(*m)[i] = newRandMetric(b, rand, i)
	}
}

func addToStore(b *testing.B, items int, m []*Metric, s *Store) {
	b.Helper()
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
				b.Helper()
				fillMetric(b, rand, items, m, s)
				addToStore(b, items, *m, s)
			},
			b: func(b *testing.B, items int, m []*Metric, s *Store) {
				b.Helper()
				s.Range(func(*Metric) error {
					return nil
				})
			},
		},
	}
	rand := rand.New(rand.NewSource(99))
	for _, bench := range benches {
		bench := bench
		for _, gc := range []bool{false, true} {
			gc := gc
			gcStr := ""
			if gc {
				gcStr = "WithGc"
			}
			for _, parallel := range []bool{false, true} {
				parallel := parallel
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

func newRandLabels(tb testing.TB, rand *rand.Rand, i int) []string {
	tb.Helper()
	lv := make([]string, i)
	for j := 0; j < i; j++ {
		val, ok := quick.Value(reflect.TypeOf(""), rand)
		if !ok {
			tb.Fatalf("%d-%d: can't make a label", i, j)
		}
		lv[j] = val.Interface().(string)
	}
	return lv
}

func fillLabel(b *testing.B, rand *rand.Rand, items, keys int, lvs *[][]string, _ *Metric) {
	b.Helper()
	for i := 0; i < items; i++ {
		(*lvs)[i] = newRandLabels(b, rand, keys)
	}
}

func getDatum(b *testing.B, items int, lvs *[][]string, m *Metric) {
	b.Helper()
	for j := 0; j < items; j++ {
		lv := (*lvs)[j]
		m.GetDatum(lv...)
	}
}

type metricBench struct {
	name  string
	setup func(b *testing.B, rand *rand.Rand, items, keys int, lvs *[][]string, m *Metric)
	b     func(b *testing.B, items int, lv *[][]string, m *Metric)
}

func BenchmarkMetric(b *testing.B) {
	maxKeys := 4
	benches := []metricBench{
		{
			name:  "GetDatum",
			setup: fillLabel,
			b:     getDatum,
		},
	}
	rand := rand.New(rand.NewSource(99))
	for _, bench := range benches {
		bench := bench
		for _, parallel := range []bool{false, true} {
			parallel := parallel
			parallelStr := ""
			if parallel {
				parallelStr = "Parallel"
			}

			for i := 1; i <= maxLabelsLog2; i++ {
				items := int(math.Pow(2, float64(i)))
				lv := newRandLabels(b, rand, maxKeys)
				b.Run(fmt.Sprintf("%s%s-%d", bench.name, parallelStr, items), func(b *testing.B) {
					m := NewMetric("test", "prog", Counter, Int, lv...)
					lvs := make([][]string, items)
					if bench.setup != nil {
						bench.setup(b, rand, items, maxKeys, &lvs, m)
					}
					b.ResetTimer()
					if parallel {
						b.RunParallel(func(pb *testing.PB) {
							for pb.Next() {
								bench.b(b, items, &lvs, m)
							}
						})
					} else {
						for n := 0; n < b.N; n++ {
							bench.b(b, items, &lvs, m)
						}
					}
				})
			}
		}
	}
}
