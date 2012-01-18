# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

/$/ {                                          # 4: match /$/
                                               # 5: jnm 9
  inc(line-count)                              # 6: inc line-count
}                                              # 7: ret true
