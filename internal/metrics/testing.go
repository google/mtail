// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

type MetricSlice []*Metric

func (s MetricSlice) Len() int      { return len(s) }
func (s MetricSlice) Swap(i, j int) { s[i], s[j] = s[j], s[i] }
func (s MetricSlice) Less(i, j int) bool {
	return Less(s[i], s[j])
}

func Less(m1, m2 *Metric) bool {
	if m1.Name < m2.Name {
		return true
	}
	if m1.Name > m2.Name {
		return false
	}
	if m1.Program < m2.Program {
		return true
	}
	if m1.Program > m2.Program {
		return false
	}
	if m1.Kind < m2.Kind {
		return true
	}
	if m1.Kind > m2.Kind {
		return false
	}
	if m1.Type < m2.Type {
		return true
	}
	if m1.Type > m2.Type {
		return false
	}
	if len(m1.Keys) < len(m2.Keys) {
		return true
	}
	if len(m1.Keys) > len(m2.Keys) {
		return false
	}
	for x, k := range m1.Keys {
		if k < m2.Keys[x] {
			return true
		}
		if k > m2.Keys[x] {
			return false
		}
	}
	for x, lv := range m1.LabelValues {
		if len(lv.Labels) < len(m2.LabelValues[x].Labels) {
			return true
		}
		if len(lv.Labels) > len(m2.LabelValues[x].Labels) {
			return false
		}
		for y, k := range lv.Labels {
			if k < m2.LabelValues[x].Labels[y] {
				return true
			}
			if k > m2.LabelValues[x].Labels[y] {
				return false
			}
		}
		// if lv.Value < m2.LabelValues[x].Value {
		// 	return true
		// }
	}
	return false
}
