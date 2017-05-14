// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp/syntax"
	"testing"
)

var groupOnlyMatchesTests = []struct {
	pattern  string
	check    string
	expected bool
}{
	{`\d+`,
		"0123456789",
		true,
	},
	{`[0123456789]`,
		"0123456789",
		true,
	},
	{`(0|1|2|3|4|5|6|7|8|9)`,
		"0123456789",
		true,
	},
	{`(\+|-)?\d+(\.\d+)?`,
		"0123456789",
		false,
	},
	{`(\+|-)?\d+(\.\d+)?`,
		"0123456789.eE+-",
		true,
	},
}

func TestGroupOnlyMatches(t *testing.T) {
	for _, tc := range groupOnlyMatchesTests {
		r, err := syntax.Parse(tc.pattern, syntax.Perl)
		if err != nil {
			t.Fatalf("syntax.Parse failed: %s", err)
		}
		result := groupOnlyMatches(r, tc.check)
		if result != tc.expected {
			t.Errorf("Pattern %q didn't only match check %q: expected %+v, received %+v", tc.pattern, tc.check, tc.expected, result)
		}
	}
}
