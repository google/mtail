// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"testing"
	"time"
)

func TestDatumSetAndValue(t *testing.T) {
	d := MakeInt(12, time.Unix(37, 42))
	if r := GetInt(d); r != 12 {
		t.Errorf("d ditn't return 12, got %v", r)
	}
	if r := d.Value(); r != "12" {
		t.Errorf("d value is not 12, got %v", r)
	}
	if r := d.Time(); r != "37" {
		t.Errorf("d Time not correct, got %v", r)
	}
}
