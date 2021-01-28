x <- runif(1e7)
y <- runif(1e7)

# Integer arithmetic much slower


# Surpringly, division is competitive with addition and multiplication

microbenchmark::microbenchmark(
  x^2, x*x, x+y, x^.5, x^.3, x/3.1,
  times=50, control=list(order='inorder')
)
## Unit: milliseconds
##   expr   min    lq  mean median    uq   max neval
##    x^2  18.6  19.5  33.9   20.8  22.0 127.8    50
##  x * x  15.6  16.2  17.0   17.0  17.3  20.2    50
##  x + y  18.7  20.0  33.7   20.6  21.6 126.0    50
##  x^0.5 296.1 302.1 312.4  308.3 313.8 405.9    50
##  x^0.3 295.3 300.6 324.3  305.5 319.9 511.1    50
##  x/3.1  15.0  15.4  16.6   16.3  17.2  24.0    50

# Avoiding intermediate steps can save ~50% of the times

microbenchmark::microbenchmark(
  a=(x - xu) ^ 2 + (y - yu) ^ 2,
  b={
    x_xu <- x - xu
    x_xu2 <- x ^ 2
    y_yu <- y - yu
    y_yu2 <- y ^ 2
    x_xu2 + y_yu2
  },
  control=list(order='inorder')
)
## Unit: milliseconds
##  expr  min    lq mean median  uq max neval
##     a 77.9  81.2  107   83.7 110 327   100
##     b 98.7 101.8  146  137.9 166 364   100

# Operator Overhead

microbenchmark::microbenchmark(
  x + 1, ((x + 1) + 2) + 3, (((x + 1) + 2) + 3) + 4,
  ((((x + 1) + 2) + 3) + 4) + 5,
  control=list(order='inorder')
)

# Memory allocations are not a huge issue between R and C so long as C
# needs to make at least one


# Lots of small allocations don't seem worse than one large one.  Presumably
# this is true so long as they are allocated contiguously and returned that way.
# Are the small allocations (1K) big enough to not be from R heap?


xu <- mean(x)
yu <- mean(y)

# The version with intermediate steps

microbenchmark::microbenchmark(
  a=(x - xu) ^ 2 + (y - yu) ^ 2,
  b={
    x_xu <- x - xu
    x_xu2 <- x ^ 2
    y_yu <- y - yu
    y_yu2 <- y ^ 2
    x_xu2 + y_yu2
  },
  control=list(order='inorder')
)

g <- rep(seq_len(1e3), each=1e4)
xl <- unname(split(x, g))
yl <- unname(split(x, g))

microbenchmark::microbenchmark(
  z <- x + y, w <- Map(`+`, xl, yl),
  times=100,
)

#
