package main

import (
	"time"
)

type mtype int

const (
	Counter mtype = iota
	Gauge
)

type Metric struct {
	Name  string
	Value uint64
	Time  time.Time
	Type  mtype
	Unit  string
	Tags  map[string]string
}
