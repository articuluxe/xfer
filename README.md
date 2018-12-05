xfer
====
File Transfer

# Motivation #

`xfer` is an Emacs library that helps in transferring files between locations.  Its benefits are best realized when transferring large files between hosts.  Why?

Tramp is great.  Really.  I use it all the time.  But it is not always optimal.  The degree to which it is sub-optimal is, shall we say, *optimized* when you have the great fortune of being on a Windows host.  In that case, and also but to a lesser extent on other operating systems, it can be faster to transfer files via scp on the command line than to copy them via elisp.  The difference can be dramatic the larger the file size.

## The Pudding ##

Since haters will hate and skeptics will skept, here are raw results comparing file transfers using tramp, compared to xfer.

Seconds taken to transfer 25 MB file:

|         | tramp    | xfer  |
| Windows | 2954.201 | 4.278 |
| Unix    | 281.534  | 1.243 |

You read that right, a 25 MB file used to take 49 minutes to copy onto a windows host, and now takes 4 seconds.  Please note the absolute times are of course entirely dependent on network topology, but I draw your attention to the relative improvement yielded by `pscp`, and of which `xfer` takes advantage.

On linux, `scp` allows an improvement not quite as significant, but still impressive.

Before anyone asks, this is with the following optimization settings enabled on the windows host:

    (setq w32-get-true-file-attributes nil)
    (setq w32-pipe-read-delay 0)
    (setq inhibit-compacting-font-caches t)

If you know of other settings that may make tramp faster on windows, I would be all ears!  

Part of the point of having a library like this is to take advantage of the fastest methods when available, but to fall back to slower but more universal methods when necessary.  This makes transferring files between hosts a little more brain-dead, which is a positive thing for me!  Accordingly, when copying files on the same host, `copy-file` is used; obviously `scp` is only used when it is available.  On windows, `pscp` is used when available; it is part of the [putty](https://www.putty.org "PuTTY") suite of tools.

`xfer` also tries to compress files before and after transferring, where it makes sense.  For remote hosts, this best utilizes available bandwidth.

-------------------------------------------------------------------------------

## Installation ##

Ensure xfer.el is in a directory present inside your `load-path`.

## Commands ##

