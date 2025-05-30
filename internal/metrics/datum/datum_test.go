// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/testutil"
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
		testutil.ExpectNoDiff(t, tc.expected, string(b))
	}
}
