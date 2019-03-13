// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/google/mtail/internal/testutil"
)

func TestDatumSetAndValue(t *testing.T) {
	d := MakeInt(12, time.Unix(37, 42))
	if r := GetInt(d); r != 12 {
		t.Errorf("d ditn't return 12, got %v", r)
	}
	if r := d.ValueString(); r != "12" {
		t.Errorf("d value is not 12, got %v", r)
	}
	if r := d.TimeString(); r != "37" {
		t.Errorf("d Time not correct, got %v", r)
	}
	d = MakeFloat(1.2, time.Unix(37, 42))
	if r := GetFloat(d); r != 1.2 {
		t.Errorf("d ditn't return 12, got %v", r)
	}
	if r := d.ValueString(); r != "1.2" {
		t.Errorf("d value is not 12, got %v", r)
	}
	if r := d.TimeString(); r != "37" {
		t.Errorf("d Time not correct, got %v", r)
	}
}

var datumJSONTests = []struct {
	datum    Datum
	expected string
}{
	{
		MakeInt(37, time.Unix(42, 12)),
		`{"Value":37,"Time":42000000012}`,
	},
	{
		MakeFloat(37.1, time.Unix(42, 12)),
		`{"Value":37.1,"Time":42000000012}`,
	},
}

func TestMarshalJSON(t *testing.T) {
	// This is not a round trip test because only the LabelValue knows how to unmarshal a Datum.
	for i, tc := range datumJSONTests {
		b, err := json.Marshal(tc.datum)
		if err != nil {
			t.Errorf("%d: Marshal failed: %v", i, err)
		}
		if diff := testutil.Diff(tc.expected, string(b)); diff != "" {
			t.Errorf("%d: JSON didn't match:\n%s", i, diff)
		}
	}
}

func TestType(t *testing.T) {
	if x := NewInt().Type(); x != Int {
		t.Errorf("Int type was %v, not Int", x)
	}
	if x := NewFloat().Type(); x != Float {
		t.Errorf("Float type was %v, not Float", x)
	}
	if x := NewString().Type(); x != String {
		t.Errorf("String type was %v, not String", x)

	}
	if x := NewBuckets([]Range{{0, 1}}).Type(); x != Buckets {
		t.Errorf("Buckets type was %v, not Buckets", x)
	}
}
