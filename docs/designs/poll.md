# Polling filesystem watcher

Original date: 2018-08-13

Tracking issue #169

Some users want a polling option instead of fsnotify as their platforms don't support fsnotify, e.g. mipsel (bug in fsnotify) or no kernel support? (using on AIX).

To the best of our ability, users shoud not have to configure poll- or fsnotify-based filesystem watching.

## design ideas

fsnotify watch add error, fallback to poll.  How does fsnotify report errors about watches not being supported?  E.g on NFS or with AIX?

poll implemented similar to fsnotify poll loop?  if that, will that be duplicated work?  Do we care enough to avoid nested polling loops?  should this be pushed upstream?

how to let users override the choice?  Argument listing poll-only filesystem path prefixes?



