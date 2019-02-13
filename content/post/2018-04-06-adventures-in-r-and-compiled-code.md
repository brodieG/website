---
title: Adventures in R and Compiled Code
author: ~
date: '2018-04-06'
slug: adventures-in-r-and-compiled-code
contenttype: article
description: Wherein I learn a bit more about how to properly use valgrind, and
  unexpectedly find a bug in base R.
weight: 1
image: /front-img/valgrind.png
categories: [r]
tags: [valgrind, debugging]
---

# Preface

My programming background is mostly in interpreted languages, so much of what follows will be obvious to experienced C programmers.  If you notice any errors please let me know.  I share this in the hopes it may be helpful to others taking the leap into C from R.

# Getting on CRAN

Over the past couple of years I started including C code in my R packages.  At first just by adapting existing code for use in R, and more recently by writing substantial portions of my packages in C.

I know enough about C to realize I need to be more careful with code I write in C than with code I write in R.  For my newest package I used [`covr`](https://github.com/r-lib/covr) to ensure 100% coverage of the code, ran my tests on R-devel and several earlier versions of R, on Linux, OS X, Windows (with Uwe Ligges [winbuilder](https://win-builder.r-project.org/)), and even Solaris (with Gábor Csárdi's [r-hub](https://github.com/r-hub/rhub/graphs/contributors)).  I used Tomas Kalibera's [rchk][2] to check for protection errors.  Finally I re-read [Kevin Ushey's great 2015 Post][1] about using R and `valgrind`, and ran my tests locally on OSX with `valgrind`.

I convinced myself this would be good enough and submitted to R.  A day goes by, and another, ... and then my package just shows up on CRAN.  Yes!  Except shortly afterwards I find in my inbox the following e-mail from an R Core member know not to suffer fools gladly:

> See fansi (my package) check results. valgrind is reporting the use of uninitialized strings.
>
> Please correct ASAP and before April 20 to safely retain the package on CRAN.

Oh sh##.  But I checked, I swear! I tested again with the same tarball, and again, I could not reproduce the error.  Artist rendition of Brodie shortly after failing to reproduce bug:

![The Scream](https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/The_Scream.jpg/377px-The_Scream.jpg)

<br />
# Docker to The Rescue

Thankfully not long ago I had run across [Winston Chang's r-debug docker container][3] (for a great introduction to using docker with R, see [Jim Hester's great post on docker containers](http://www.jimhester.com/2017/10/13/docker/)). 

So I sacrificed a bleating lamb to the bug-reproducibility gods and fired up `r-debug` to run my tests:

```
docker run --rm -ti -v $(pwd):/mydir wch1/r-debug
RDvalgrind -e "install.packages('/mydir/fansi_0.2.1.tar.gz')"
RDvalgrind -d valgrind  # and run tests
```

and got a bunch of errors, unlike with my OSX version of `valgrind`.  So a step in the right direction.  But here is an example of the type of error I got:

```
==518== Conditional jump or move depends on uninitialised value(s)
==518==    at 0x404DD28: ???
==518==    by 0x17F0601C: ???
==518==    by 0x17F0601C: ???
==518==    by 0x17F0601D: ???
==518==    by 0x1FFEFD151F: ???
```

At least `valgrind` is as confused as I am...  On CRAN, the errors looked like:

```
==8932== Conditional jump or move depends on uninitialised value(s)
==8932==    at 0x403EBFB: ???
==8932==    by 0x10D25C25: ???
==8932==    by 0x10D25C25: ???
==8932==    by 0x10D25DC7: ???
==8932==    by 0x1FFEFE6A5F: ???
==8932==  Uninitialised value was created by a heap allocation
==8932==    at 0x4C2DB6B: malloc (m_replacemalloc/vg_replace_malloc.c:299)
==8932==    by 0x511B31: Rf_allocVector3 (svn/R-devel/src/main/memory.c:2712)
==8932==    by 0x4B73D0: Rf_mkCharLenCE (svn/R-devel/src/main/envir.c:3913)
==8932==    by 0x1D6B5BE8: FANSI_strip (packages/tests-vg/fansi/src/strip.c:170)
... snip ...
==8932==    by 0x4DA52F: Rf_eval (svn/R-devel/src/main/eval.c:624)
==8932==
```

A lot more information, and most importantly, highlights the particular line in my code potentially responsible for the problems:
```
==8932==    by 0x1D6B5BE8: FANSI_strip (packages/tests-vg/fansi/src/strip.c:170)
```
At a loss I resorted to re-reading the [WRE section on valgrind](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Using-valgrind), and lo and behold:

> valgrind is good at spotting the use of uninitialized values: use option --track-origins=yes to show where these originated from.

So I run my tests under:

```
RDvalgrind -d "valgrind --track-origins=yes"
```

And boom: I get the exact same error message as CRAN!  Perhaps I lead a cloistered life and am easily excited, but there are few things I find as exhilarating as reproducing a hard to reproduce bug.  Reproducing the bug in a controlled environment is the climax of the debugging story.  The fix barely rates a mention in the epilogue.

# What I Learned About Reproducing `valgrind` Errors

1. Use an R version that was built with level 2 `valgrind` instrumentation.  This is what `RDvalgrind` in `r-debug` has, and this is also part of the reason I could not reproduce the errors that showed up on CRAN.  More details to follow.
2. Compile your package with the `-O0` flag (`r-debug` sets this as a default option) to minimize false positive and to get the most useful information.  `-g` is also a good setting, but R sets that by default.
3. Use `RDvalgrind -d "valgrind --track-origins=yes"` as CRAN seems to do the same, and this provides useful context.
4. Use an R-devel version as close as possible to what CRAN is using.
5. You can never re-read Writing R Extensions too many times.

Additionally, as part of the debugging process I ended up having to build R myself for more fine grained control over what version of R-devel I tested against.  I had never done this before, and It turns out that [Tomas Kalibera's excellent `rchck` vagrant image](https://github.com/kalibera/rchk) is a fantastic platform to build R in.

All I had to do was `vagrant ssh` into his image and:

```
sudo apt-get install valgrind
svn checkout https://svn.r-project.org/R/trunk
cd ~/trunk
./config --with-valgrind-instrumentation=2
make
```

And then, after setting the "-O0" package compilation setting in `~/.R/Makevars`:

```
./bin/R -d "valgrind --track-origins=yes"
```

# More on `--with-valgrind-instrumentation=2`

`--with-valgrind-instrumentation=2` is the configure option that sets up R to build with `valgrind` level 2 instrumentation.  One thing this does is it allows `valgrind` to detect memory errors in portions of memory that are wholly owned by R.  For example, R maintains an internal heap for `R_alloc` allocations of 128 bytes or fewer.  Normally `valgrind` would not detect memory errors occurring within this heap.  Here is an example from the CRAN errors:

```
==518== Invalid read of size 1
==518==    at 0x4EE519C: substr (character.c:286)
==518==    by 0x4EE563D: do_substr (character.c:342)
==518==    by 0x4F83444: bcEval (eval.c:6771)
==518==    by 0x4F70257: Rf_eval (eval.c:624)
... snip ...
==518==    by 0x4F70257: Rf_eval (eval.c:624)
==518==  Address 0xc5cb7ef is 3,087 bytes inside a block of size 7,960 alloc'd
==518==    at 0x4C2FB0F: malloc (in vgpreload_memcheck-amd64-linux.so)
==518==    by 0x4FBF2B9: GetNewPage (memory.c:888)
==518==    by 0x4FCD13C: Rf_allocVector3 (memory.c:2691)
==518==    by 0x4FB2D9C: Rf_allocVector (Rinlinedfuns.h:577)
... snip ...
==518==    by 0x4F4B1AA: duplicate1 (duplicate.c:312)
==518==
```

This is an error that you would not see without the instrumentation.  The give-away is that the read is *inside* an allocated block.  Usually `valgrind` errors involve read/writes before, after, etc., an allocated block.  The `GetNewPage` call in the trace highlights this is related to R's internal memory pages.

Thankfully I could tell from the trace that nothing in this error involved compiled code from my package...  

_OMG I think I found a bug in R_.

It turns out that I triggered [a bug in `base::substr`](https://stat.ethz.ch/pipermail/r-devel/2018-March/075774.html) with corner case UTF-8 tests that were part of my package.

# Epilogue

None of the errors showing up on CRAN were actually caused by my package.  Most of these errors except for the R bug went away with different R-devel builds.  [Details for the bored](https://gist.github.com/brodieG/d364807792883c6a15006fec8d307def).

However, as a result of my new found understanding of these memory errors I went back to look at an old `valgrind` error in another package of mine.  I had ignored it because I did not understand it, I could not reproduce it, nothing was obviously blowing up, and no one was yelling at me about it.  Sure enough, the error was real, and was able to fix before anyone else ran into it.

<div id='feedback-cont'></div>

# Acknowledgments

Figuring all this out would have been impossible without all the work others put in and freely shared.  Thank you to:

* R core for writing WRE in as much detail as they have, and CRAN maintainers for running a tight ship.
* Winston Chang for the [r-debug][3] docker container.
* Gábor Csárdi for helping confirm my interpretation of the `valgrind` error messages.
* Kevin Ushey for the [blog post about valgrind in R][1].
* Tomas Kalibera for his [rchk image][2].


[1]: https://kevinushey.github.io/blog/2015/04/05/debugging-with-valgrind/
[2]: https://github.com/kalibera/rchk
[3]: https://github.com/wch/r-debug

