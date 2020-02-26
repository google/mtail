package testutil

import (
	"fmt"
	"os/user"
	"testing"
)

func SkipIfRoot(tb testing.TB) {
	tb.Helper()
	u, err := user.Current()
	if err != nil {
		tb.Skip(fmt.Sprintf("Couldn't determine current user id: %s", err))
	}
	if u.Uid == "0" {
		tb.Skip("Skipping test when run as root")
	}
}
