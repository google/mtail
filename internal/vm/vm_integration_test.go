// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"bufio"
	"context"
	"math"
	"strings"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
)

var vmTests = []struct {
	name          string
	prog          string
	log           string
	runtimeErrors int64
	metrics       map[string][]*metrics.Metric
}{
	{"single-dash-parseint",
		`counter c

/(?P<x>-)/ {
    $x == "-" {
        c++
    }
}
`, `123 a
- b
`,
		0,
		map[string][]*metrics.Metric{
			"c": {
				{
					Name:    "c",
					Program: "single-dash-parseint",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Hidden:  false,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 1},
						},
					},
				},
			},
		},
	},
	{"histogram",
		`histogram hist1 buckets 1, 2, 4, 8
histogram hist2 by code buckets 0, 1, 2, 4, 8
histogram hist3 by f buckets -1, 0, 1

/^(.) (\d+)/ {
  hist1 = $2
  hist2[$1] = $2
}

/^(?P<foo>[a-z]+) (?P<time>\d+)$/ {
  hist3[$foo] = $time
}
`,
		`b 3
b 3
b 3
`,
		0,
		map[string][]*metrics.Metric{
			"hist1": {
				{
					Name:    "hist1",
					Program: "histogram",
					Kind:    metrics.Histogram,
					Type:    metrics.Buckets,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Buckets{
								Buckets: []datum.BucketCount{
									{Range: datum.Range{Min: 0, Max: 1}},
									{Range: datum.Range{Min: 1, Max: 2}},
									{Range: datum.Range{Min: 2, Max: 4},
										Count: 3},
									{Range: datum.Range{Min: 4, Max: 8}},
									{Range: datum.Range{Min: 8, Max: math.Inf(+1)}},
								},
								Count: 3,
								Sum:   9,
							},
						},
					},
					Buckets: []datum.Range{{Min: 0, Max: 1}, {Min: 1, Max: 2}, {Min: 2, Max: 4}, {Min: 4, Max: 8}, {Min: 8, Max: math.Inf(+1)}},
				},
			},
			"hist2": {
				&metrics.Metric{
					Name:    "hist2",
					Program: "histogram",
					Kind:    metrics.Histogram,
					Type:    metrics.Buckets,
					Keys:    []string{"code"},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{"b"},
							Value: &datum.Buckets{
								Buckets: []datum.BucketCount{
									{Range: datum.Range{Min: 0, Max: 1}},
									{Range: datum.Range{Min: 1, Max: 2}},
									{Range: datum.Range{Min: 2, Max: 4},
										Count: 3},
									{Range: datum.Range{Min: 4, Max: 8}},
									{Range: datum.Range{Min: 8, Max: math.Inf(+1)}},
								},
								Count: 3,
								Sum:   9,
							},
						},
					},
					Buckets: []datum.Range{{Min: 0, Max: 1}, {Min: 1, Max: 2}, {Min: 2, Max: 4}, {Min: 4, Max: 8}, {Min: 8, Max: math.Inf(+1)}},
				},
			},
			"hist3": {
				&metrics.Metric{
					Name:    "hist3",
					Program: "histogram",
					Kind:    metrics.Histogram,
					Type:    metrics.Buckets,
					Keys:    []string{"f"},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{"b"},
							Value: &datum.Buckets{
								Buckets: []datum.BucketCount{
									{Range: datum.Range{Min: -1, Max: 0}},
									{Range: datum.Range{Min: 0, Max: 1}},
									{Range: datum.Range{Min: 1, Max: math.Inf(+1)},
										Count: 3},
								},
								Count: 3,
								Sum:   9,
							},
						},
					},
					Buckets: []datum.Range{{Min: -1, Max: 0}, {Min: 0, Max: 1}, {Min: 1, Max: math.Inf(+1)}},
				},
			},
		},
	},
	{"numbers",
		`counter error_log_count

/^/ +
/(?P<date>\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}:\d{2}) / +
/.*/ +
/$/ {
    strptime($date, "2006/01/02 15:04:05")

    error_log_count++
}
`,
		`2019/05/14 11:10:05 [warn] ...
2019/05/14 11:11:06 [warn] ...
`,
		0,
		map[string][]*metrics.Metric{
			"error_log_count": {
				{
					Name:    "error_log_count",
					Program: "numbers",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{
								Value: 2,
							},
						},
					},
				},
			},
		},
	},
	{"parse a hyphen",
		`counter total
/^[a-z]+ ((?P<response_size>\d+)|-)$/ {
  $response_size > 0 {
    total = $response_size
  }
}`,
		`test 99
test -
`,
		1,
		map[string][]*metrics.Metric{
			"total": {
				{
					Name:    "total",
					Program: "parse a hyphen",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{
								Value: 99,
							},
						},
					},
				},
			},
		},
	},
	{"parse around a hyphen",
		`counter total
/^[a-z]+ ((?P<response_size>\d+)|-)$/ {
  $1 != "-" {
    total = $response_size
  }
}`,
		`test 99
test -
`,
		0,
		map[string][]*metrics.Metric{
			"total": {
				{
					Name:    "total",
					Program: "parse around a hyphen",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{
								Value: 99,
							},
						},
					},
				},
			},
		},
	},
	{
		"add_assign_float",
		`gauge metric
/(\d+\.\d+)/ {
  metric += $1
}
`, `1.1
`,
		0,
		map[string][]*metrics.Metric{
			"metric": {
				{
					Name:    "metric",
					Program: "add_assign_float",
					Kind:    metrics.Gauge,
					Type:    metrics.Float,
					Hidden:  false,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{},
							Value:  &datum.Float{Valuebits: math.Float64bits(1.1)},
						},
					},
				},
			},
		},
	},
	{
		"decorator",
		`counter a
counter b
counter c

def decoratora {
  /(...).*/ {
    next
  }
}

def decoratorb {
  /(?P<x>...).*/ {
    next
  }
}

# This tests that the variables in the decorator are visible to the decoratedo block.
@decoratora {
  $1 == "Dec" {
    a++
  }
}

@decoratorb {
  $x == "Dec" {
    b++
  }
}

/(...).*/ {
  $1 == "Dec" {
     c++
  }
}
`, `Dec
Jan
Dec
Apr
Dec
Oct
`,
		0,
		map[string][]*metrics.Metric{
			"a": {
				{
					Name:    "a",
					Program: "decorator",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Hidden:  false,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 3},
						},
					},
				},
			},
			"b": {
				{
					Name:    "b",
					Program: "decorator",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Hidden:  false,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 3},
						},
					},
				},
			},
			"c": {
				{
					Name:    "c",
					Program: "decorator",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Hidden:  false,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 3},
						},
					},
				},
			},
		},
	},
	{"else",
		`counter yes
counter maybe
counter no

/1/ {
  /^1$/ {
    yes++
  } else {
    maybe++
  }
} else {
  no++
}
`, `1
2
12
3
4
991
`, 0,
		map[string][]*metrics.Metric{
			"yes": {
				{
					Name:    "yes",
					Program: "else",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 1},
						},
					},
				},
			},
			"maybe": {
				{
					Name:    "maybe",
					Program: "else",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 2},
						},
					},
				},
			},
			"no": {
				{
					Name:    "no",
					Program: "else",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 3},
						},
					},
				},
			},
		},
	},
	{"otherwise",
		`counter yes
counter maybe
counter no

/1/ {
  /^1$/ {
    yes++
  }
  otherwise {
    maybe++
  }
}
otherwise {
  no++
}
`, `1
2
12
3
4
991
`, 0,
		map[string][]*metrics.Metric{
			"yes": {
				{
					Name:    "yes",
					Program: "otherwise",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 1},
						},
					},
				},
			},
			"maybe": {
				{
					Name:    "maybe",
					Program: "otherwise",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 2},
						},
					},
				},
			},
			"no": {
				{
					Name:    "no",
					Program: "otherwise",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 3},
						},
					},
				},
			},
		},
	},
	{"types",
		`gauge should_be_int
gauge should_be_float
counter neg
gauge should_be_float_map by label
gauge should_be_int_map by label
counter i

/^(\d+)$/ {
  should_be_int = $1
  should_be_int_map[$1] = $1
}

/^(\d+\.\d+)$/ {
  should_be_float = $1
  should_be_float_map[$1] = $1
}


/(?P<bar>[+-]?[\d.]+)/ {
  $bar < -1 {
    neg++
  }
}

/^(\d+)$/ {
  # Sneaky float promotion
  i += 1.0 * $1
}
`, `37
12.8
`,
		0,
		map[string][]*metrics.Metric{
			"should_be_int": {
				{
					Name:    "should_be_int",
					Program: "types",
					Kind:    metrics.Gauge,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{},
							Value:  &datum.Int{Value: 37},
						},
					},
				},
			},
			"should_be_float": {
				{
					Name:    "should_be_float",
					Program: "types",
					Kind:    metrics.Gauge,
					Type:    metrics.Float,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{},
							Value:  &datum.Float{Valuebits: math.Float64bits(12.8)},
						},
					},
				},
			},
			"should_be_int_map": {
				{
					Name:    "should_be_int_map",
					Program: "types",
					Kind:    metrics.Gauge,
					Type:    metrics.Int,
					Keys:    []string{"label"},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{"37"},
							Value:  &datum.Int{Value: 37},
						},
					},
				},
			},
			"should_be_float_map": {
				{
					Name:    "should_be_float_map",
					Program: "types",
					Kind:    metrics.Gauge,
					Type:    metrics.Float,
					Keys:    []string{"label"},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{"12.8"},
							Value:  &datum.Float{Valuebits: math.Float64bits(12.8)},
						},
					},
				},
			},
			"neg": {
				{
					Name:    "neg",
					Program: "types",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{},
						},
					},
				},
			},
			"i": {
				{
					Name:    "i",
					Program: "types",
					Kind:    metrics.Counter,
					Type:    metrics.Float,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Float{Valuebits: math.Float64bits(37.0)},
						},
					},
				},
			},
		},
	},
	{"filename",
		`counter filename_lines by filename

// {
    filename_lines[getfilename()] ++
}
`, `1
2
12
3
4
991
`,
		0,
		map[string][]*metrics.Metric{
			"filename_lines": {
				{
					Name:    "filename_lines",
					Program: "filename",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{"filename"},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{"filename"},
							Value:  &datum.Int{Value: 6},
						},
					},
				},
			},
		},
	},
}

func TestVmEndToEnd(t *testing.T) {
	if testing.Verbose() {
		testutil.SetFlag(t, "vmodule", "vm=2,loader=2,checker=2")
	}
	for _, tc := range vmTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			progRuntimeErrorsCheck := testutil.ExpectMapExpvarDeltaWithDeadline(t, "prog_runtime_errors_total", tc.name, tc.runtimeErrors)

			store := metrics.NewStore()
			lines := make(chan *logline.LogLine, 1)
			var wg sync.WaitGroup
			l, err := NewLoader(lines, &wg, "", store, ErrorsAbort(), DumpAst(), DumpAstTypes(), DumpBytecode(), OmitMetricSource())
			testutil.FatalIfErr(t, err)
			compileErrors := l.CompileAndRun(tc.name, strings.NewReader(tc.prog))
			testutil.FatalIfErr(t, compileErrors)
			scanner := bufio.NewScanner(strings.NewReader(tc.log))
			lineCount := 0
			for scanner.Scan() {
				lineCount++
				lines <- logline.New(context.Background(), tc.name, scanner.Text())
			}
			close(lines)
			wg.Wait()

			progRuntimeErrorsCheck()
			testutil.ExpectNoDiff(t, tc.metrics, store.Metrics, testutil.IgnoreUnexported(sync.RWMutex{}), testutil.IgnoreFields(datum.BaseDatum{}, "Time"))
		})
	}
}
