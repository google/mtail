package main

import (
        "time"
)

type mtype int

const (
        counter mtype   = iota
        gauge
)

type metric struct {
        name    string
        value   float64
        time    time.Time
        typ     mtype
        unit    string
        tag     map[string]string
}
