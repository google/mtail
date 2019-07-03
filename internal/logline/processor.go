// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package logline

import "context"

// Processor is an interface for processing LogLines.
type Processor interface {
	ProcessLogLine(context.Context, *LogLine)
}
