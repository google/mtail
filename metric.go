// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"time"
)

type mtype int

const (
	Counter mtype = iota
	Gauge
)

func (m mtype) String() string {
	switch m {
	case Counter:
		return "Counter"
	case Gauge:
		return "Gauge"
	}
	return "Unknown"
}

type Metric struct {
	Name  string
	Value int64
	Time  time.Time
	Type  mtype
	Unit  string
	Tags  map[string]string
}
