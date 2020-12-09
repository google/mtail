package testutil

import (
	"testing"
	"time"

	"github.com/google/mtail/internal/vm/errors"
)

func TestDoOrTimeoutNeverOK(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	// Never return OK so timeout at 10ms.
	ok, err := DoOrTimeout(func() (bool, error) {
		return false, nil
	}, 10*time.Millisecond, time.Millisecond)
	if ok || err != nil {
		t.Errorf("Expected timeout (false, nil), got %v, %v", ok, err)
	}
}

func TestDoOrTimeoutAlwaysOK(t *testing.T) {
	// Always return OK.
	ok, err := DoOrTimeout(func() (bool, error) {
		return true, nil
	}, 10*time.Millisecond, time.Millisecond)
	if !ok || err != nil {
		t.Errorf("Expected OK, got %v, %v", ok, err)
	}
}

func TestDoOrTimeoutStallThenOK(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	// Stall for 5 ticks (50ms) and then return OK; timeout at 1s.
	i := 5
	ok, err := DoOrTimeout(func() (bool, error) {
		i--
		if i > 0 {
			return false, nil
		}
		return true, nil
	}, time.Second, 10*time.Millisecond)
	if !ok || err != nil {
		t.Errorf("Expected OK, got %v, %v", ok, err)
	}
}

func TestDoOrTimeoutAlwaysErr(t *testing.T) {
	// Return an error
	ok, err := DoOrTimeout(func() (bool, error) {
		return false, errors.Errorf("oh no")
	}, 10*time.Millisecond, time.Millisecond)
	if ok || err == nil {
		t.Errorf("Expected !ok and an error, got %v %v", ok, err)
	}
}
