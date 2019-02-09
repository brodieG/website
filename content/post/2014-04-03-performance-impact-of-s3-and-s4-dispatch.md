---
title: Performance Impact of S3 and S4 Dispatch
author: Brodie Gaslam
date: '2014-04-03'
slug: performance-impact-of-s3-and-s4-dispatch
summary: Is the S3 and S4 method dispatch penalty sufficient to matter?
weight: 1
categories: [r]
tags: [optimization]
---

# The Setup

I have known for a while that S3 and S4 dispatch carries a performance penalty, but I have never run into a situation where it is a glaring problem.  I am currently contemplating writing some functions that could potentially be called many times, so I decided to benchmark S3 and S4 dispatch.  The quick answer is that the performance impact of method dispatch is unlikely to matter most of the time (for an example of an exception see the "When Does it Matter?" section).

The tests are structured to evaluate direct calls to the functions, as well as a couple of variations on dispatch.  They were executed on a OSX 10.8 / 2.0GHz Dual Core i7 / 8GB RAM system as well as on Win7 / 2.8GHz Quad Core Xeon / 6GB RAM system (both with R3.0.2-Rstudio).

# S3 Methods

```{r eval=FALSE}
myfun <- function(x, ...) UseMethod("myfun")
myfun.default <- function(x, ...) x
myfun.test1 <- function(x, ...) x                  # returns directly
myfun.test2 <- function(x, ...) NextMethod()       # dispatches again

# Objects to Dispatch

x <- "hello"
y <- structure("hello", class="test1")
z <- structure("hello", class="test2")

library(microbenchmark)
microbenchmark(
  myfun(x),               # default method
  myfun(y),               # "test1" method, no further dipatch
  myfun(z),               # "test2" method, re-dispatch to default
  myfun.default(x)        # bypass S3 dispatch
)
```
```
Unit: nanoseconds
             expr    min       lq   median       uq    max neval
         myfun(x)   2886   3535.5   3990.5   5194.0   7427   100
         myfun(y)   2298   2769.0   3095.5   3796.0   5499   100
         myfun(z)   5125   6034.0   6730.0   7744.5  38117   100
 myfun.default(x)    346    446.0    588.5    722.0   1317   100   # baseline
```

Clearly method dispatch carries a performance penalty relative to the baseline case.  The simplest dispatch scenarios (first and second here) add on the order of 3&mu;s to execution.  While the extra amount of time is small on an absolute basis, it is a ~5x increase over baseline.  Interestingly, the unclassed object (i.e. the one that ends up at `myfun.default`) is almost 25% slower to dispatch than the classed one<sup>1</sup>.  Invoking `NextMethod` as `myfun(z)` results in another roughly 2x increase.  This makes sense since we're using S3 dispatch twice.

# S4 Methods

```{r eval=FALSE}
setGeneric("myfun2", function(x, ...) standardGeneric("myfun2"))
setClass("testS41", representation(x="character"))
setClass("testS42", contains="testS41")

setMethod("myfun2", "testS42", function(x, ...) callNextMethod()) # redispatch 
setMethod("myfun2", "testS41", function(x, ...) x@x)              # direct return
myfun2S41 <- selectMethod("myfun2", "testS41")                    # manually retrieve method

w <- new("testS41", x="hello")
u <- new("testS42", x="hello")

library(microbenchmark)
microbenchmark(
  myfun2(w),             # "testS41" method, no additional dispatch 
  myfun2(u),             # with redispatch 
  myfun2S41(w)           # baseline
)
```
```
Unit: nanoseconds
             expr    min       lq   median       uq    max neval
        myfun2(w)  11751  13943.0  15127.5  16231.0  42846   100
        myfun2(u) 146687 150092.5 155058.5 162427.0 332575   100
     myfun2S41(w)    456    621.5    709.5    803.5   1580   100  # baseline
```

The simplest S4 dispatch creates a ~20x increase over baseline.  Using `callNextMethod` you go up to a ~200x increase in execution time over base line.  This is on a clean workspace with only the default packages and `microbenchmark` loaded.

Method look up in S4 is a lot more complex than in S3, so one would expect a performance penalty between S3 and S4.  What is truly surprising is what happens with `callNextMethod`.  The logic within `callNextMethod` is fairly complex and uses several calls to functions such as `is` that presumably do lookups on the S4 tables, so perhaps this should be expected<sup>2</sup>.

# When Is This a Problem?

You are unlikely to notice S3 and S4 dispatch in most use cases, especially if you properly vectorize your functions.  Even our slowest benchmark ran in about an eight of a millisecond.  One type of situation that could cause problems is if your function is used as part of a [split-apply-combine analysis](http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/).  Consider this example that takes advantage of S4 dispatch to apply the correct method to each column of a data frame.  We benchmark that approach to a more traditional `if` control flow based function:

```{r eval=FALSE}
# S4 dispatch approach

setGeneric("vecconcat", function(x) standardGeneric("vecconcat"))
setMethod("vecconcat", "numeric", function(x) sum(x))
setMethod("vecconcat", "character", function(x) paste0(x, collapse=":"))
setMethod("vecconcat", "factor", function(x) { x <- as.numeric(x); callNextMethod() })

# Traditional `if` approach

vecconcat2 <- function(x) {
  if(inherits(x, "factor")) x <- as.integer(x)
  if(inherits(x, "integer")) sum(x) else
  if(inherits(x, "character")) paste0(x, collapse=":")
}
# Some Data

set.seed(1)
df <- data.frame(
  grp.id=rep(1:1e4, each=10), b=sample(1e5), c=sample(letters, 1e5, r=T), 
  d=factor(sample(letters, 1e5, r=T)), stringsAsFactors=F
)
# Benchmarks

microbenchmark(times=5, unit="s",
  aggregate(df[-1], df[1], vecconcat),
  aggregate(df[-1], df[1], vecconcat2)
)
```
```
Unit: seconds
                                 expr  min   lq median   uq  max neval
  aggregate(df[-1], df[1], vecconcat) 5.66 5.70   5.71 5.71 5.71     5
 aggregate(df[-1], df[1], vecconcat2) 2.91 2.98   3.01 3.02 3.04     5
```

So about 2x slower using S4 dispatch.  It turns out that a fair chunk of the ~3 seconds of the traditional function run is taken up by `aggregate` itself.  If we use the more efficient `data.table`, the difference is closer to 10x because now there is a lot less overhead from the aggregation function itself:

```{r eval=FALSE}
library(data.table)
dt <- data.table(df)
mb1 <- microbenchmark(times=5, unit="s",
  dt[, lapply(.SD, vecconcat), by=grp.id], 
  dt[, lapply(.SD, vecconcat2), by=grp.id]
)
```
```
Unit: seconds
                                       expr  min    lq median    uq   max neval
  dt[, lapply(.SD, vecconcat), by = grp.id] 2.87 2.883  2.908 2.911 2.922     5
 dt[, lapply(.SD, vecconcat2), by = grp.id] 0.25 0.255  0.256 0.258 0.258     5
```

Some might argue that there is no benefit whatsoever to the S4 approach so the slowness is moot, but even in this example there are some disguised benefits.  For example, the S4 code will work with both numeric and integer columns, whereas the "traditional" approach only works with integer.  More importantly, there may be functions that have more legitimate internal use for S4 methods that would still carry the same performance overhead as this top level dispatch.

# Conclusions

Most of the time you won't have to worry about S3/S4 dispatch, but if you plan on developing with S3 or S4 make sure you carefully think through the use cases to ensure there won't be any that will cause the extra overhead from method dispatch to add up to substantial extra time.

One big caveat is that we are using fairly simple S4 dispatch here.  It is possible that with complex webs of S4 classes dispatch times could become an issue in more traditional use cases.

<div id='feedback-cont'></div>

---
<div class="footnotes">
<sup>1</sup> At first I thought this could indicate that S3 generics with lots of methods could be particularly affected, but the slowdown seems to be specifically with the invocation of the default method, not with looking through a large method list.<br />
<sup>2</sup> I replicated this result on a windows machine as well, suggesting this overhead is real.
</div>
