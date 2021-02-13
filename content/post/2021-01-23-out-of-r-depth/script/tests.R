set.seed(1)
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

xu <- mean(x)
yu <- mean(y)
microbenchmark::microbenchmark(
  a=(x - xu) ^ 2 + (y - yu) ^ 2,
  b={
    x_xu <- x - xu
    x_xu2 <- x ^ 2
    y_yu <- y - yu
    y_yu2 <- y ^ 2
    x_xu2 + y_yu2
  },
  control=list(order='block')
)
## Unit: milliseconds
##  expr  min    lq mean median  uq max neval
##     a 77.9  81.2  107   83.7 110 327   100
##     b 98.7 101.8  146  137.9 166 364   100

# Operator Overhead, need to have explicit garbage collections in-between each
# as otherwise things get messy maybe because of the rare level 1 and 2 GCs that
# happen once or twice every ~100 iterations.
#
# Suggests allocation cost is ~2/3 of a minimal calculation.

library(microbenchmark)
options(digits=3)
gc()
microbenchmark(x + x)
gc()
microbenchmark((x + x) + x)
gc()
microbenchmark(((x + x) + x) + x)
gc()
microbenchmark((((x + x) + x) + x) + x)
gc()
microbenchmark(((((x + x) + x) + x) + x) + x)
mean.times <- c(23.6, 37.9, 54.6, 67.1, 81.8)
xx <- 1:5
lm(mean.times ~ xx)

## Call:
## lm(formula = mean.times ~ xx)
## 
## Coefficients:
## (Intercept)           xx  
##        9.32        14.56  

x1e0 <- x[1]
x5e0 <- x[seq_len(5e0)]
x1e1 <- x[seq_len(1e1)]
x2e1 <- x[seq_len(2e1)]
x4e1 <- x[seq_len(4e1)]
x5e1 <- x[seq_len(5e1)]
x1e2 <- x[seq_len(1e2)]

x1e2 <- x[seq_len(1e2)]
x1e3 <- x[seq_len(1e3)]
x1e4 <- x[seq_len(1e4)]
x1e5 <- x[seq_len(1e5)]
x1e6 <- x[seq_len(1e6)]
x1e7 <- x[seq_len(1e7)]

gc()
microbenchmark(x1e0 * 2, times=5e6)   # 176e-9
gc()
microbenchmark(x5e0 * 2, times=2e6)   # 224e-9
gc()
microbenchmark(x1e1 * 2, times=1e6)   # 259e-9
gc()
microbenchmark(x2e1 * 2, times=1e6)   # 594e-9
gc()
microbenchmark(x4e1 * 2, times=1e6)   # 788e-9
gc()
microbenchmark(x5e1 * 2, times=1e6)   # 872e-9
gc()
microbenchmark(x1e2 * 2, times=5e5)   # 1237e-9

microbenchmark(
  x1e0 * 2, 
  x5e0 * 2, 
  x1e1 * 2, 
  x2e1 * 2, 
  x4e1 * 2, 
  x5e1 * 2, 
  x1e2 * 2, 
  times = 2e6,
  control=list(order='block')
)

n <- c(2,5,10,20,40,50,100)
time <- c(176,224,259,594,788,872,1237) * 1e-9
plot(n, time)


gc()
microbenchmark(x1e3 * 2, times=1e5)
gc()
microbenchmark(x1e4 * 2, times=1e4)
gc()
microbenchmark(x1e5 * 2, times=1e3)
gc()
microbenchmark(x1e6 * 2, times=5e2)
gc()
microbenchmark(x1e7 * 2, times=5e2)

mt <- c(184e-9, 252e-9, 1173e-9, 5720e-9, 58.3e-6, 524e-6,5.39e-3, 25.4e-3)
mtx <- c(1,     1e1,        1e2,     1e3,     1e4,     1e5,   1e6,   1e7)

plot(log(mtx), log(mt))


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

water <- find_water(map)
water.small <- find_water(downsample(map, factor=10))
dim(water.small)

water.i <- which(water.small, arr.ind=TRUE)
land.i <- which(!water.small, arr.ind=TRUE)

system.time(
# for(i in seq_len(nrow(water.i)))
{
dist <- rep(Inf, nrow(water.i))
for(i in seq_len(nrow(water.i))) {
  w <- water.i[i,]
  for(j in seq_len(nrow(land.i))) {
    d <- sum((w - land.i[j,]) ^ 2)
    if(dist[i] > d) dist[i] <- d
  }
}
}
)
##   user  system elapsed 
## 73.253   0.405  74.769 

# C version implies one distance calculation per 2.05 nanoseconds, so a little
# about 2.5 cycles per for:
# * 4 loads
# * 2 substractions
# * 2 squares
# * 1 addition
# * 1 comparison
# * 1 load
# * 1/5000th of a store
#
# But basically doing a simple sum of a full vector into a single value takes
# just about a long.
#
# Some research suggests memory bandwidth could be ~25GB/s, which is ~20 bytes
# per cycle, which is 2.5 floating point numbers per cycle.  I'm also seeing as
# low as 12GB/s, or possibly higher if there are 4 instead of 2 channels?
#
# But there is weirdness because for the calculations we're loading 4x as much
# data as for the simple sum?  Ah, not at all, a bunch of stuff will reside in
# cache for the depth calculation, but for the simple sum we have to fetch the
# entire thing from memory.

dyn.unload('script/depth.so')
xx <- dyn.load('script/depth.so')

library(microbenchmark)
w <- water.small + 0
gc()
microbenchmark(.Call('BG_calc_depth', w, shore.i, water.i), times=25)
## Unit: milliseconds
##                                                       expr  min   lq mean
##  .Call("BG_calc_depth", water.small + 0, shore.i, water.i) 70.3 72.4 74.5
##  median uq max neval
##    72.7 74 108    25

y <- runif(prod(table(water.small)))

gc()
microbenchmark(.Call('BG_sum', y), times=25)
## Unit: milliseconds
##                expr min   lq mean median   uq  max neval
##  .Call("BG_sum", y)  60 62.8 64.5   63.1 63.9 91.5    25

md <- array(0, dim(water.small))
md[water.i] <- dist

## Try the full vectorized version.  The slow step is the rowsums, of
## which most is the subsetting?



sytem.time(
{
water.j <- t(water.i)
land.j <- t(land.i)
i <- rep(seq_len(nrow(water.i)), nrow(land.i))
j <- rep(seq_len(nrow(land.i)), each=nrow(water.i))
dists <- rowSums((water.i[i,] - land.i[j,]) ^ 2)
dim(dists) <- c(nrow(water.i), nrow(land.i))
dist <- dists[cbind(seq_len(nrow(water.i)), max.col(-dists, 'first'))]
}
)

gc(); microbenchmark::microbenchmark(rowSums(mx), times=25)
gc(); microbenchmark::microbenchmark(colSums(mx), times=25)

dyn.unload('script/depth.so')
xx <- dyn.load('script/depth.so')
mx <- matrix(runif(5e3^2), 5e3)
gc(); microbenchmark::microbenchmark(res <- .Call("BG_rowsums", mx), times=25)
gc(); microbenchmark::microbenchmark(res <- .Call("BG_rowsums2", mx), times=25)
gc(); microbenchmark::microbenchmark(res3 <- .Call("BG_rowsums3", mx), times=25)
gc(); microbenchmark::microbenchmark(res4 <- .Call("BG_rowsums4", mx), times=25)
gc(); microbenchmark::microbenchmark(res5 <- .Call("BG_rowsums5", mx), times=25)
gc(); microbenchmark::microbenchmark(res2 <- rowSums(mx), times=25)
gc(); microbenchmark::microbenchmark(t(mx), times=25)

dyn.unload('script/depth.so')
xx <- dyn.load('script/depth.so')
mx2 <- matrix(runif(16), 4, 4)
res5 <- .Call("BG_rowsums5", mx2)
res5 <- .Call("BG_rowsums5", mx)

colSums

# colsums instead of rowsums
system.time(
{
water.j <- t(water.i)
land.j <- t(land.i)
i <- rep(seq_len(ncol(water.j)), ncol(land.j))
j <- rep(seq_len(ncol(land.j)), each=ncol(water.j))
dists <- colSums((water.j[,i] - land.j[,j]) ^ 2)
dim(dists) <- c(nrow(water.i), nrow(land.i))
# dist <- dists[cbind(seq_len(nrow(water.i)), max.col(-dists, 'first'))]
dist <- apply(dists, 1, min)
}
)

a <- water.i[i,]
b <- land.i[j,]

aa <- t(a)
bb <- t(b)

system.time((a - b)^2)

dist <- rep(Inf, nrow(water.i))
dist <- numeric(nrow(water.i))
system.time({
land.j <- t(land.i)
for(i in seq_len(nrow(water.i))) {
  dist[i]<- min(colSums((water.i[i,] - land.j)^2))
}
}
)
# timing suggests about 2 ops per clock
# timing of a single += operation without allocation suggests about 1
# op per clock for simple addition, x += x * x also about 1 op per
# clock which suggests fused ops may be a factor, though not entirely
# clear how that plays into the distance calc.
#
# A simple R addition, attempting to exclude the allocation assuming
# it takes 2/3 of the time of the addition (i.e `time / (1 + 2/3)`)
# suggests 3-4 cycles per element for R.  This could be affected by
# a big gc (and made slower).

system.time(
mc <- .Call('BG_calc_depth', water.small+0, land.i, water.i)
)

# Interestingly the very clever iteration mechanism for arithmetic
# operations does not seem to add any overhead with simple sums
# occuring at about one per clock cycle.  But how does that explain
# the computed 1 addition per 2-3 clock cycles for the naive
# calculation?  Is there more going on with the memory allocation that
# we're missing?
#
# Simple addition is really the same whether we do it in R or via C,
# so long as we allocate the result vector.  So something more
# complicated is going on with the distance calculation.  Must be that
# the different operations involved can better use the ALUs, but that
# can't happen with R because all the ops are serialized.
#
# Updated, did it again and I'm getting 1 addition per 1.8 clock
# cycles estimated based on these timings for 1:7:
#
# times <- c(154, 201, 239, 279, 315, 350, 389)
# 
# lm(times ~ x)
# 
# Call:
# lm(formula = times ~ x)
# 
# Coefficients:
# (Intercept)            x  
#      121.14        38.54  
#
# Hmm, this is quite different to what we had seen previously in terms
# of mem-alloc vs incremental.  This is larger and maybe busy session?
#
# Hmm, still seems true at 1e7?  Yes, the difference is the allocation
# is much slower per unit when request 2.5e7 vs 1e7.

x <- runif(1e7)
library(microbenchmark)
gc(); microbenchmark(x + x, times=25)
gc(); microbenchmark(x + x + x, times=25)
gc(); microbenchmark(x + x + x + x, times=25)
gc(); microbenchmark(x + x + x + x + x, times=25)
gc(); microbenchmark(x + x + x + x + x + x, times=25)
gc(); microbenchmark(x + x + x + x + x + x + x, times=25)
gc(); microbenchmark(x + x + x + x + x + x + x + x, times=25)

times <- c(27.25, 43.02, 59.47,72.07, 88.01, 103.41,121.43)  # 1e7
times <- c(28, 43, 57, 69, 85, 107, 117)                     # 1e7
times <- c(154, 186, 227, 261, 305, 334, 377)                # 2.5e7
x <- 1:7
lm(times ~ x)
15.42e-3 / 1e7 / (1/1.2e9)

mean(x)
system.time(x <- .Call('BG_double_iter', x))
mean(x)
system.time(x <- .Call('BG_double', x))

dyn.unload('content/post/2021-01-23-out-of-r-depth/script/depth.so')
xx <- dyn.load('content/post/2021-01-23-out-of-r-depth/script/depth.so')

gc(); microbenchmark(.Call('BG_mult_add', x, x, x, x))
gc(); microbenchmark(.Call('BG_mult_add2', x, x, x, x))
gc(); microbenchmark((x * x) + (x * x))


# We get very close to the actual time required by allocating a temp vector for
# the intermediate multiplication, which suggests the R trickery probably can't
# do this.  There is also the question of how much overhead is from the loop
# itself.
#
# So ultimately the problem is we lose access to the either multiply/add
# goodness + loop overhead, or parallel use of ALUs (the latter shouldn' really
# be the case, we should be able to do that with OO execution of sequence),
# require an additional allocation for the temporary branch.

# I don't understand why the following time differently.  From stepping with GDB
# it looks like both formulations are able to re-use the internal vector.

x + (x + x)
(x + x) + x

# On further testing I can't reproduce the slow runs.

