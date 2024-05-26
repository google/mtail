// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package waker_test

import (
	"context"
	"sync"
	"testing"

	"github.com/google/mtail/internal/waker"
)

func TestTestWakerWakes(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w, awaken := waker.NewTest(ctx, 1, "test")
	c := w.Wake()
	select {
	case x := <-c:
		t.Errorf("<-w.Wake() == %v, expected nothing (should block)", x)
	default:
	}
	awaken(1, 0)
	select {
	case <-c:
		// Luke Luck likes lakes.  Luke's duck likes lakes.
	default:
		t.Errorf("<-w.Wake() blocked, expected close")
	}
}

func TestTestWakerTwoWakees(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w, awaken := waker.NewTest(ctx, 2, "test")
	var wg1, wg2, wg3 sync.WaitGroup
	wg1.Add(1)
	wg2.Add(1)
	wg3.Add(1)
	go func() {
		defer wg3.Done()
		c := w.Wake()
		select {
		case x := <-c:
			t.Errorf("<-w.Wake() == %v, expected nothing (should block)", x)
		default:
		}
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
	}()
	wg1.Done()
	awaken(2, 0) // wake 2, and await none
	wg2.Done()
	wg3.Wait()
}

func TestTestWakerTwoWakeups(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	w, awaken := waker.NewTest(ctx, 1, "test")
	s := make(chan struct{})
	begin := make(chan struct{})
	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		<-begin
		for i := 0; i < 2; i++ {
			c := w.Wake()
			select {
			case x := <-c:
				t.Errorf("<-w.Wake() == %v, expected nothing (should block), pass %d", x, i)
			default:
			}
			s <- struct{}{}
			<-c // wait to receive the wake
		}
	}()
	go func() {
		defer wg.Done()
		<-begin
		<-s
		awaken(1, 1) // awaken 1, wait for 1
		<-s
		// we don't expect anyone to call Wake() after this
		awaken(1, 0)
	}()
	close(begin)
	wg.Wait()
}
