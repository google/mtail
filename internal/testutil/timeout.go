package testutil

import (
	"time"

	"github.com/golang/glog"
)

func DoOrTimeout(do func() (bool, error), deadline, interval time.Duration) (bool, error) {
	timeout := time.After(deadline)
	ticker := time.Tick(interval)
	for {
		select {
		case <-timeout:
			return false, nil
		case <-ticker:
			glog.V(2).Infof("tick")
			ok, err := do()
			glog.V(2).Infof("ok, err: %v %v", ok, err)
			if err != nil {
				return false, err
			} else if ok {
				return true, nil
			}
		}
	}
}
