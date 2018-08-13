# Polling filesystem watcher

Original date: 2018-08-13

Tracking issue #169

Some users want a polling option instead of fsnotify as their platforms don't support fsnotify, e.g. mipsel (bug in fsnotify) or no kernel support? (using on AIX).

To the best of our ability, users shoud not have to configure poll- or fsnotify-based filesystem watching.

From Linux's inotify(7):

       Inotify  reports  only events that a user-space program triggers through the filesystem API.  As a result,
       it does not catch remote events that occur on  network  filesystems.   (Applications  must  fall  back  to
       polling  the  filesystem  to  catch  such events.)  Furthermore, various pseudo-filesystems such as /proc,
       /sys, and /dev/pts are not monitorable with inotify.

## design ideas

fsnotify watch add error, fallback to poll.  How does fsnotify report errors about watches not being supported?  E.g on NFS or with AIX?

poll implemented similar to fsnotify poll loop?  if that, will that be duplicated work?  Do we care enough to avoid nested polling loops?  should this be pushed upstream?

how to let users override the choice?  Argument listing poll-only filesystem path prefixes?

Could poll be on by default for all files, with a timeout if no events have been received from inotify in some timeout?  This could be tricky, we don't need to poll files that are inotified.  But, again from inotify(7):

       Note that the event queue can overflow.  In this case, events are lost.  Robust applications should handle
       the possibility of lost events gracefully.  For example, it may be necessary to rebuild part or all of the
       application cache.  (One simple, but possibly expensive, approach is to close the inotify file descriptor,
       empty the cache, create a new inotify file descriptor, and then re-create watches and  cache  entries  for
       the objects to be monitored.)


## references

https://github.com/fsnotify/fsnotify

inotify(7)

