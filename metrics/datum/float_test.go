// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/kylelemons/godebug/pretty"
)

func TestFloatMarshalJSON(t *testing.T) {
	d := MakeInt(37, time.Unix(42, 12))
	b, err := json.Marshal(d)
	if err != nil {
		t.Errorf("Marshal failed: %v", err)
	}
	expected := `{"Value":37,"Time":42000000012}`
	if diff := pretty.Compare(expected, string(b)); len(diff) > 0 {
		t.Errorf("JSON didn't match:\n%s", diff)
	}
}
