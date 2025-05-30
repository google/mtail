// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"fmt"
	"runtime"
)

// BuildInfo records the compile-time information for use when reporting the mtail version.
type BuildInfo struct {
	Branch   string
	Version  string
	Revision string
	EmbedLabel string
}

func (b BuildInfo) String() string {
	label := ""
	if b.EmbedLabel != "" {
		label = fmt.Sprintf(" label %s", b.EmbedLabel)
	}
	return fmt.Sprintf(
		"mtail version %s git revision %s go version %s go arch %s go os %s",
		b.Version,
		b.Revision,
		runtime.Version(),
		runtime.GOARCH,
		runtime.GOOS,
	) + label
}
