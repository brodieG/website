---
title: A Toolkit For Fast R
author: ~
date: '2018-10-27'
slug: a-toolkit-for-fast-r
draft: true
image: /front-img/default.png
contenttype: article
description: R has a reputation for being slow, which can be true if you are not
  aware of the intended use patterns.  We cover some general concepts and tips
  and tricks to keep your R code speedy.
categories: [optimization]
tags: [rstats]
---

# Must Know Concepts

* Favor internal recycling (i.e. `runif(1e6) * (5 * pi)` instead of
  `(runif(1e6) * 5) * pi`
* Recycling with matrices
* Using lists instead of matrices for column data to avoid subset/copy overhead
    * but OMG data.frame row-names

# Must Know Functions

* `col`
* `row`
* `filter`
* `rle`
* `outer`, but watch out for memory
* `embed`
* `%*%`
* `* * *` faster than `^3`
* `rowSums` and friends (include > 2d examples)
* `rowsum`, need to check how fast this actual is for matrices
* `max.col`
* `cumsum` and friends
* `t`
* `rep`, where `length(x) == length(times)`
* arithmetic operators
* `sum`, `mean`, etc.
* `sort`, `order`, etc.
* `which`, `which.min`, also `arrInd=TRUE` examples
* `asplit`, not sure, new in R3.6.0

# Example

* Group sums
* Group max
* Multiply

# References

WinVector's [FastBaseR][1]

[1]: https://github.com/WinVector/FastBaseR
