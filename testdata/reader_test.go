package testdata

import (
	"fmt"
	"os"
	"testing"

	"github.com/google/mtail/metrics"
)

func TestReadTestData(t *testing.T) {
	f, err := os.Open("rsyncd.golden")
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	store := &metrics.Store{}
	ReadTestData(f, "rsyncd", store)
	fmt.Printf("%v\n", store.Metrics)
}
