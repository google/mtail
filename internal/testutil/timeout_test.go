package testutil

import (
	"testing"
	"time"
)

func TestDoOrTimeout(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	ok, err := DoOrTimeout(func() (bool, error) {
		return false, nil
	}, 10*time.Millisecond, time.Millisecond)
	if ok || err != nil {
		t.Errorf("Expected timeout (false, nil), got %v, %v", ok, err)
	}

	i := 5
	ok, err = DoOrTimeout(func() (bool, error) {
		i--
		if i > 0 {
			return false, nil
		}
		return true, nil
	}, 100*time.Millisecond, time.Millisecond)
	if !ok || err != nil {
		t.Errorf("Expected OK, got %v, %v", ok, err)
	}

	ok, err = DoOrTimeout(func() (bool, error) {
		return true, nil
	}, 10*time.Millisecond, time.Millisecond)
	if !ok || err != nil {
		t.Errorf("Expected OK, got %v, %v", ok, err)
	}
}
