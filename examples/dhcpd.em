# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

/(?P<date>(?P<legacy_date>\w+\s+\d+\s+\d+:\d+:\d+)|(?P<rfc3339_date>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d+[+-]\d{2}:\d{2}))\s+'(?:\w+@)?(?P<hostname>[\w\.-]+)\s+(?P<application>[\w\.-]+)(?:\[(?P<pid>\d+)\])?:\s+(?P<message>.*)/ {
  strptime($2, "Mon Jan _2 15:04:05")
  
  /(\w+)/ {
    
  }
}
