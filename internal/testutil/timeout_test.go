package testutil

import (
	"testing"
	"time"
)

func TestDoOrTimeout(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	ok, err := doOrTimeout(func() (bool, error) {
		return false, nil
	}, 10*time.Millisecond, time.Millisecond)
	if ok || err == nil {
		t.Errorf("Expected timeout, got %v, %v", ok, err)
	}

	i := 5
	ok, err = doOrTimeout(func() (bool, error) {
		i--
		if i > 0 {
			return false, nil
		}
		return true, nil
	}, 100*time.Millisecond, time.Millisecond)
	if !ok || err != nil {
		t.Errorf("Expected OK, got %v, %v", ok, err)
	}

	ok, err = doOrTimeout(func() (bool, error) {
		return true, nil
	}, 10*time.Millisecond, time.Millisecond)
	if !ok || err != nil {
		t.Errorf("Expected OK, got %v, %v", ok, err)
	}
}
