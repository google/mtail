#!/usr/bin/env bash

echo STABLE_GIT_BRANCH $(git --no-pager rev-parse --abbrev-ref HEAD)
echo STABLE_GIT_VERSION $(git --no-pager describe --tags --always --dirty)
echo STABLE_GIT_REVISION $(git --no-pager rev-parse HEAD)

# Convert a version + commit count into a SemVer.
echo STABLE_GIT_SEMVER $(git --no-pager describe --tags | sed -e 's/^v//' -e 's/-/./' -e 's/-.*$//')

# volatiles
# RFC 3339 ISO 8601 format for the OCI image.
echo BUILD_TIMESTAMP_ISO8601 $(date --iso-8601=seconds --utc | sed -e 's/+00:00/Z/')
