---
title: A Toolkit For Fast R
author: ~
date: '2018-10-27'
slug: a-toolkit-for-fast-r
draft: true
categories: [optimization]
tags: [rstats]
---

# Must Know Concepts

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

# Example

* Group sums
* Group max
* Multiply 

