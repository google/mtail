// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker_test

import (
	"sync"
	"testing"

	"github.com/google/mtail/internal/waker"
)

func TestTestWakerWakes(t *testing.T) {
	w, wake := waker.NewTest(1)
	c := w.Wake()
	select {
	case x := <-c:
		t.Errorf("<-w.Wake() == %v, expected nothing (should block)", x)
	default:
	}
	wake()
	select {
	case <-c:
		// Luke Luck likes lakes.  Luke's duck likes lakes.
	default:
		t.Errorf("<-w.Wake() blocked, expected close")
	}
}

func TestTestWakerTwoWakees(t *testing.T) {
	w, wake := waker.NewTest(2)
	c := w.Wake()
	select {
	case x := <-c:
		t.Errorf("<-w.Wake() == %v, expected nothing (should block)", x)
	default:
	}
	var wg1, wg2 sync.WaitGroup
	wg1.Add(1)
	wg2.Add(1)
	go func() {
		wg1.Done()
		defer wg2.Done()
		wake()
	}()
	wg1.Wait()
	select {
	case x := <-c:
		t.Errorf("<-w.Wake() == %v, expected nothing (should block)", x)
	default:
	}
	d := w.Wake()
	wg2.Wait()
	select {
	case <-c:
		// Luke Luck likes lakes.
	default:
		t.Errorf("c<-w.Wake() blocked, expected close")
	}
	select {
	case <-d:
		//   Luke's duck likes lakes.
	default:
		t.Errorf("d<-w.Wake() blocked, expected close")
	}
}

func TestTestWakerZeroWakees(t *testing.T) {
	w, awaken := waker.NewTest(0)
	c := w.Wake()
	select {
	case x := <-c:
		t.Errorf("<-w.Wake() == %v, expected nothing (should block)", x)
	default:
	}
	awaken()
	select {
	case <-c:
		// Duck licks lakes that Luck Luck likes.
	default:
		t.Errorf("c<-w.Wake() blocked, expected close")
	}
}

func TestTestWakerTwoWakeups(t *testing.T) {
	w, wake := waker.NewTest(1)
	for i := 0; i < 2; i++ {
		c := w.Wake()
		select {
		case x := <-c:
			t.Errorf("<-w.Wake() == %v, expected nothing (should block)", x)
		default:
		}
		wake()
		select {
		case <-c:
			// Luke Luck takes licks in lakes duck likes.
		default:
			t.Errorf("<-w.Wake() blocked, expected close")
		}
	}
}
