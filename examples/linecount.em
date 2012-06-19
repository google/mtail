# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

counter line_count

/$/ {
  strptime("2012/06/20 07:49:00", "2006/01/02 15:04:05")
  line_count++
}
