// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

import (
	"bufio"
	"context"
	"math"
	"strings"
	"sync"
	"testing"

	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
)

var vmTests = []struct {
	name    string
	prog    string
	log     string
	errs    int64
	metrics metrics.MetricSlice
}{
	{
		"single-dash-parseint",
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
		metrics.MetricSlice{
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
	{
		"histogram",
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
b 0
`,
		0,
		metrics.MetricSlice{
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
								{
									Range: datum.Range{Min: 0, Max: 1},
									Count: 1,
								},
								{Range: datum.Range{Min: 1, Max: 2}},
								{
									Range: datum.Range{Min: 2, Max: 4},
									Count: 3,
								},
								{Range: datum.Range{Min: 4, Max: 8}},
								{Range: datum.Range{Min: 8, Max: math.Inf(+1)}},
							},
							Count: 4,
							Sum:   9,
						},
					},
				},
				Buckets: []datum.Range{{Min: 0, Max: 1}, {Min: 1, Max: 2}, {Min: 2, Max: 4}, {Min: 4, Max: 8}, {Min: 8, Max: math.Inf(+1)}},
			},
			{
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
								{
									Range: datum.Range{Min: 0, Max: 1},
									Count: 1,
								},
								{Range: datum.Range{Min: 1, Max: 2}},
								{
									Range: datum.Range{Min: 2, Max: 4},
									Count: 3,
								},
								{Range: datum.Range{Min: 4, Max: 8}},
								{Range: datum.Range{Min: 8, Max: math.Inf(+1)}},
							},
							Count: 4,
							Sum:   9,
						},
					},
				},
				Buckets: []datum.Range{{Min: 0, Max: 1}, {Min: 1, Max: 2}, {Min: 2, Max: 4}, {Min: 4, Max: 8}, {Min: 8, Max: math.Inf(+1)}},
			},

			{
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
								{
									Range: datum.Range{Min: -1, Max: 0},
									Count: 1,
								},
								{Range: datum.Range{Min: 0, Max: 1}},
								{
									Range: datum.Range{Min: 1, Max: math.Inf(+1)},
									Count: 3,
								},
							},
							Count: 4,
							Sum:   9,
						},
					},
				},
				Buckets: []datum.Range{{Min: -1, Max: 0}, {Min: 0, Max: 1}, {Min: 1, Max: math.Inf(+1)}},
			},
		},
	},
	{
		"numbers",
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
		metrics.MetricSlice{
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
	{
		"parse a hyphen",
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
		metrics.MetricSlice{
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
	{
		"parse around a hyphen",
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
		metrics.MetricSlice{
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
	{
		"add_assign_float",
		`gauge metric
/(\d+\.\d+)/ {
  metric += $1
}
`, `1.1
`,
		0,
		metrics.MetricSlice{
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
		metrics.MetricSlice{
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
	{
		"else",
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
		metrics.MetricSlice{
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
	{
		"otherwise",
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
		metrics.MetricSlice{
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
	{
		"types",
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
		metrics.MetricSlice{
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
	{
		"filename",
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
		metrics.MetricSlice{
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
	{
		"logical operators",
		`counter foo
counter bar

# To make ex_test.go happy
strptime("2017-10-03T20:14:42Z", "2006-01-02T15:04:05Z07:00")

/(?P<var>.*)/ {
  $var == "foo" || $var == "bar" {
    foo++
  }
  $var == "bar" && 1 == 1 {
    bar++
  }
}
`, `foo
foo
bar
bar
quux
12.8

`,
		0,
		metrics.MetricSlice{
			{
				Name:    "foo",
				Program: "logical operators",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Value: &datum.Int{Value: 4},
					},
				},
			},
			{
				Name:    "bar",
				Program: "logical operators",
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
	},
	{
		"strcat",
		`counter f by s

/(.*), (.*)/ {
  f[$1 + $2]++
}
`, `a, b
c, d
`,
		0,
		metrics.MetricSlice{
			{
				Name:    "f",
				Program: "strcat",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{"s"},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{"ab"},
						Value:  &datum.Int{Value: 1},
					},
					{
						Labels: []string{"cd"},
						Value:  &datum.Int{Value: 1},
					},
				},
			},
		},
	},
	{
		"typed-comparison",
		`counter t by le
counter t_sum

/^(?P<v>\d+(\.\d+)?)/ {
    $v < 0.5 {
            t["0.5"]++
    }
    $v < 1 {
            t["1"]++
    }
    t["inf"]++
    t_sum += $v
}
`, `0.1
1
1.765
`,
		0,
		metrics.MetricSlice{
			{
				Name:    "t",
				Program: "typed-comparison",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{"le"},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{"0.5"},
						Value:  &datum.Int{Value: 1},
					},
					{
						Labels: []string{"1"},
						Value:  &datum.Int{Value: 1},
					},
					{
						Labels: []string{"inf"},
						Value:  &datum.Int{Value: 3},
					},
				},
			},
			{
				Name:    "t_sum",
				Program: "typed-comparison",
				Kind:    metrics.Counter,
				Type:    metrics.Float,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Value: &datum.Float{Valuebits: math.Float64bits(2.865)},
					},
				},
			},
		},
	},
	{
		"match-expression",
		`counter someas
counter notas
counter total

/(.*)/ {
  $1 =~ /a/ {
    someas++
  }
  $1 !~ /a/ {
    notas++
  }
  total++
}
`, `a
b
abba
baba
cdf
`,
		0,
		metrics.MetricSlice{
			{
				Name:    "someas",
				Program: "match-expression",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Value: &datum.Int{Value: 3},
					},
				},
			},
			{
				Name:    "notas",
				Program: "match-expression",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Value: &datum.Int{Value: 2},
					},
				},
			},
			{
				Name:    "total",
				Program: "match-expression",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Value: &datum.Int{Value: 5},
					},
				},
			},
		},
	},
	{
		"metric-as-rvalue",
		`gauge response_time
counter hit
counter miss

/seconds = (?P<response_seconds>\d+)/ {
    response_time = $response_seconds * 1000
    response_time < 100000 {
        hit++
    } else {
        miss++
    }
}
`, `seconds = 100
seconds = 200
seconds = 50
`,
		0,
		metrics.MetricSlice{
			{
				Name:    "hit",
				Program: "metric-as-rvalue",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Value: &datum.Int{Value: 1},
					},
				},
			},
			{
				Name:    "miss",
				Program: "metric-as-rvalue",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Value: &datum.Int{Value: 2},
					},
				},
			},
			{
				Name:    "response_time",
				Program: "metric-as-rvalue",
				Kind:    metrics.Gauge,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{},
						Value:  &datum.Int{Value: 50000},
					},
				},
			},
		},
	},
	{
		"stringy",
		`text str
counter b by foo

/(.*)/ {
  str = $1
}

/b/ {
  b[str]++
}
`, `1.1
c
b
a
`,
		0,
		metrics.MetricSlice{
			{
				Name:    "str",
				Program: "stringy",
				Kind:    metrics.Text,
				Type:    metrics.String,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{},
						Value:  &datum.String{Value: "a"},
					},
				},
			},
			{
				Name:    "b",
				Program: "stringy",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{"foo"},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{"b"},
						Value:  &datum.Int{Value: 1},
					},
				},
			},
		},
	},
	{
		"ip-addr",
		`text ipaddr

/ip address (\d+\.\d+\.\d+\.\d+)/ {
    ipaddr = $1
}
`, `ip address 1.1.1.1
`, 0,
		metrics.MetricSlice{
			{
				Name:    "ipaddr",
				Program: "ip-addr",
				Kind:    metrics.Text,
				Type:    metrics.String,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{},
						Value:  &datum.String{Value: "1.1.1.1"},
					},
				},
			},
		},
	},
	{
		name: "subst timestamp",
		prog: `gauge val
/(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2}) .*: (\d+)/ {
  strptime(subst("_", " ", $1), "2006-01-02 15:04:05")
  val = $2
}`,
		log: `2021-01-03_17:34:23 CUL_MAXCUBE_FRONT credit10ms: 3494
`,
		errs: 0,
		metrics: metrics.MetricSlice{
			{
				Name:    "val",
				Program: "subst timestamp",
				Kind:    metrics.Gauge,
				Type:    metrics.Int,
				Keys:    []string{},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{},
						Value:  &datum.Int{Value: 3494},
					},
				},
			},
		},
	},
	{
		name: "subst integer",
		prog: `counter bytes_total by dir
/sent (?P<sent>[\d,]+) bytes  received (?P<received>[\d,]+) bytes/ {
    # Sum total bytes across all sessions for this process
    bytes_total["sent"] += int(subst(",", "", $sent))
    bytes_total["received"] += int(subst(",", "", $received))
}`,
		log: `Jun 28 20:44:32 backup rsync[3996]: sent 642,410,725 bytes  received 14,998,522,122 bytes  497,002.00 bytes/sec
`,
		errs: 0,
		metrics: metrics.MetricSlice{
			{
				Name:    "bytes_total",
				Program: "subst integer",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{"dir"},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{"sent"},
						Value:  &datum.Int{Value: 642410725},
					},
					{
						Labels: []string{"received"},
						Value:  &datum.Int{Value: 14998522122},
					},
				},
			},
		},
	},
	{
		name: "regexp replace",
		prog: `hidden text route
	counter http_requests_total by method, route

	/(?P<method>\S+) (?P<url>\S+)/ {
	    route = subst(/\/\d+/, "/:num", $url)
	    http_requests_total[$method, route]++
	}
	`,
		log: `GET /v1/read/10001
	GET /v1/users/1001/orders/2001
	`,
		errs: 0,
		metrics: metrics.MetricSlice{
			{
				Name:    "http_requests_total",
				Program: "regexp replace",
				Kind:    metrics.Counter,
				Type:    metrics.Int,
				Keys:    []string{"method", "route"},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{"GET", "/v1/read/:num"},
						Value:  &datum.Int{Value: 1},
					},
					{
						Labels: []string{"GET", "/v1/users/:num/orders/:num"},
						Value:  &datum.Int{Value: 1},
					},
				},
			},
		},
	},
	{
		name: "match a pattern in a binary expr",
		prog: `const N /n/
N {
}
N && 1 {
}
`,
		log: `
`,
		errs:    0,
		metrics: nil,
	},
}

func TestRuntimeEndToEnd(t *testing.T) {
	testutil.SkipIfShort(t)
	if testing.Verbose() {
		testutil.SetFlag(t, "vmodule", "vm=2,loader=2,checker=2")
	}
	for _, tc := range vmTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			progRuntimeErrorsCheck := testutil.ExpectMapExpvarDeltaWithDeadline(t, "prog_runtime_errors_total", tc.name, tc.errs)

			store := metrics.NewStore()
			lines := make(chan *logline.LogLine, 1)
			var wg sync.WaitGroup
			r, err := New(lines, &wg, "", store, ErrorsAbort(), DumpAst(), DumpAstTypes(), DumpBytecode(), OmitMetricSource(), TraceExecution())
			testutil.FatalIfErr(t, err)
			compileErrors := r.CompileAndRun(tc.name, strings.NewReader(tc.prog))
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

			var ms metrics.MetricSlice
			store.Range(func(m *metrics.Metric) error {
				ms = append(ms, m)
				return nil
			})

			// Ignore the datum.Time field as well, as the results will be unstable otherwise.
			testutil.ExpectNoDiff(t, tc.metrics, ms, testutil.SortSlices(metrics.Less), testutil.IgnoreUnexported(metrics.Metric{}, sync.RWMutex{}, datum.String{}), testutil.IgnoreFields(datum.BaseDatum{}, "Time"))
		})
	}
}
