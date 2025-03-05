// Copyright 2023 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

// SourceMapping represents a mapping from a log source to programs.
type SourceMapping struct {
	Source   string   `yaml:"source" json:"source"`     // Log file pattern
	Programs []string `yaml:"programs" json:"programs"` // Program names that should process this source
}

// SourceMappingConfig is a collection of source mappings.
type SourceMappingConfig struct {
	Mappings       []SourceMapping `yaml:"mappings" json:"mappings"`
	UnmappedBehavior string        `yaml:"unmapped_behavior" json:"unmapped_behavior"` // "all" or "none"
}