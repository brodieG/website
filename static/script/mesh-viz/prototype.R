source('static/script/mesh-viz/rtin-vec2.R')
source('static/script/mesh-viz/rtin-vec.R')
source('static/script/mesh-viz/viz-lib.R')

# File originally from http://tylermw.com/data/dem_01.tif.zip
eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

# map <- elmat1[1:5, 1:5]
# map <- elmat1[1:17, 1:17]
# map <- elmat1[1:257, 1:257]
# map <- elmat1[1:5, 1:9]
# map <- elmat1[1:(2*4+1), 1:(2*3+1)]  # smallest error?
# map <- elmat1[1:(2*5+1), 1:(2*4+1)]
map <- elmat1[1:(2*3+1), 1:(2*4+1)]
# map <- elmat1[1:11, 1:15]
map <- elmat1[1:13, 1:11]
# map <- volcano
# map <- elmat1[-1,]
map <- elmat1[1:257,1:257]
system.time(errors <- compute_error(map))
system.time(errors <- compute_error3(map))

# debug(extract_mesh2)
tol <- diff(range(map)) / 50

system.time(trisa <- extract_mesh2(errors, tol))
system.time(trisb <- extract_mesh3(errors, tol))
# treeprof::treeprof((tris <- extract_mesh2(errors, tol))

plot_tri_ids(trisa, dim(errors))
plot_tri_ids(trisb, dim(errors))
plot_points_ids(which(errors > tol), dim(map))

microbenchmark::microbenchmark(
  {mx[pass,];mx[!pass,]},
  {lapply(ls, '[', pass); lapply(ls, '[', !pass)}
)
microbenchmark::microbenchmark(
  {wp <- which(pass); mx[wp,];mx[-wp,]},
  {wp <- which(pass); lapply(ls, '[', wp); lapply(ls, '[', -wp)},
  !pass, which(pass)
)
n <- 100
arr <- array(numeric(n*4), dim=c(n, 2, 2))
ls <- matrix(replicate(4, numeric(n), simplify=FALSE), 2, 2)
pass <- sample(c(TRUE,FALSE), n, rep=TRUE)
fail <- !pass
microbenchmark::microbenchmark(
  a={
    ap <- arr[pass,,,drop=FALSE]
    af <- arr[fail,,,drop=FALSE]
    px <- arr[,1,1]
    py <- arr[,1,2]
    tx <- arr[,2,1]
    ty <- arr[,2,2]
  },
  b={
    lsf <- lsp <- ls
    lsp[] <- lapply(ls, '[', pass)
    lsf[] <- lapply(ls, '[', fail);
    px <- lsp[[1,1]]
    py <- lsp[[1,2]]
    tx <- lsp[[2,1]]
    ty <- lsp[[2,2]]
  },
  !pass
)

# # debugging code
# writeLines(
#   paste0(
#     'terrain = [', paste0(map, collapse=','),
#     sprintf('];
#     JSON.stringify(comp_errors(terrain, %d, %d));
#     ', nrow(map), nrow(map) - 1
# ) ) )
# errors3 <- array(unlist(jsonlite::fromJSON(json)), dim(map))

# For each error, we need to draw all the corresponding triangles.  This means
# we need to figure out the size of the "diagonal", and whether we're dealing
# with a diamond or square.


# we need to compute even level odd level tiles.  For odd tiles (1 index), we
# check the actual diagonal, i.e. tile is square.  For even tiles, we treat the
# tile as a rhombus so the diagonal is actually parallel to the x axis (or y
# axis depending on how you draw it)?

# Version for animation
#
# We'll want to track:
#
# * ax, ay, bx, by,
# * cx, cy, mx, my
# * az, bz, mz
# * *Error?
# * lcx, lcy, rcx, rcy
# * errors array
#
# We'll display the errors array, and the terrain array overlaid with the
# triangle tracking.

map <- elmat1[1:5, 1:5]
map <- elmat1[1:17, 1:17]
map <- elmat1[1:257, 1:257]
system.time(errors2 <- errors_rtin(map))
errors <- compute_error(map)
all.equal(errors, errors2)

set.seed(1221)
m0 <- map[1:65, 1:65]
m1 <- map[1:129, 1:129]
m2 <- map
m3 <- matrix(round(runif(513*513) * 100, 0), 513)
m4 <- matrix(round(runif(1025*1025) * 100, 0), 1025)
m5 <- matrix(round(runif(2049*2049) * 100, 0), 2049)
m6 <- matrix(round(runif(4097*4097) * 100, 0), 4097)
m7 <- matrix(round(runif(8193*8193) * 100, 0), 8193)

stop()
mm <- m1
bench::mark(
  compute_errorc(mm, nrow(mm)),
  compute_error(mm), compute_error2(mm),
  compute_error3(mm)
)

mm <- m7
gc()
system.time(compute_error(m7))
gc()
system.time(compute_error3(m7))
gc()
system.time(compute_errorc(m7, nrow(m7)))

bench::mark(
  compute_error3(m5), compute_error(m4),
)
treeprof::treeprof(compute_error3(m6))


# compare 257 extracted mesh with tol == 10, confirmed equal, but
# now we have issue that at size 1025 JS is one order of magnitude faster

json <- '~/Downloads/error-js-1025.json'
errors3 <- array(unlist(jsonlite::fromJSON(json)), dim(map))
tri.json <- '~/Downloads/rtin-tests/tris-257.json'
tri.js.257 <- unlist(jsonlite::fromJSON(tri.json))
tri.js <- tri.js.257[seq_len(min(which(tri.js.257==0) - 1))]

tri.r <- unlist(extract_mesh3(compute_error(m2), 10))

order_by_3s <- function(x) {
  id <- rep(seq_len(length(x)/3), each=3)
  xo <- matrix(x[order(x, id, x)], 3)
  xo[, order(xo[1,])]
}

all.equal(order_by_3s(tri.r), order_by_3s(tri.js))

## Extract mesh benchmarks

# # compare to those in prototype.js (~90ms)
# > e4 <- compute_error(m4)
# > system.time(extract_mesh3(e4, 50))
#    user  system elapsed
#   0.302   0.142   0.467

treeprof::treeprof(extract_mesh3(e4, 50))

## Before we try to go to matrix list
# Ticks: 2915; Iterations: 8; Time Per: 570.5 milliseconds; Time Total: 4.564 seconds; Time Ticks: 2.915
# 
#                         milliseconds
# extract_mesh3 ------ : 570.5 -   0.0
#     lapply --------- : 206.3 - 206.3
#     next_children -- : 149.1 -  67.9
#     |   c ---------- :  42.9 -  42.9
#     |   lapply ----- :  38.2 -  38.2
#     extract_tris --- : 135.6 -  61.4
#     |   rbind ------ :  73.4 -  73.4
#     .pass_fail ----- :  78.7 -  78.7
# > microbenchmark::microbenchmark(ww <- extract_mesh3(e4, 50), times=5)
# Unit: milliseconds
#                         expr      min       lq     mean   median       uq
#  ww <- extract_mesh3(e4, 50) 419.5901 430.9858 482.0166 434.1018 559.4842
#       max neval
#  565.9209     5

# Performance is very much driven by how many triangles are returned.  For
# example with m4 and tolerance of 95, ex_mesh3 is at 8ms vs 16ms for the JS
# thing.
#
# Some timings before switching to tracking delta instead of parent
#
# > system.time(ww <- extract_mesh3(err4, 50))
#    user  system elapsed 
#   0.242   0.110   0.361 
# > system.time(ww <- extract_mesh3(err4, 50))
#    user  system elapsed 
#   0.242   0.110   0.353 

n <- 617252
m <- 444246
n <- 6100
m <- 4400
id <- replicate(4, sample(n), simplify=FALSE)
id2 <- lapply(id, `+`, 0)
x <- logical(n)
x[sample(n, m)]<-TRUE
y <- !x
wx <- which(x)
wy <- which(y)

f <- function() {
  lsp3 <- vector('list', 4L)
  lsf3 <- vector('list', 4L)
  for(i in seq_len(4L)) {
    lsp3[[i]] <- id[[i]][x]
    lsf3[[i]] <- id[[i]][y]
  }
  lsf3
}
wx <- which(x)
bench::mark(
  #a={
  #  ap <- id2[,x,drop=FALSE]
  #  af <- id2[,y,drop=FALSE]
  #  pxa <- ap[1,]
  #  pya <- ap[2,]
  #  txa <- af[1,]
  #  tya <- af[2,]
  #},
  #a2={
  #  lsp3 <- vector('list', 4L)
  #  lsf3 <- vector('list', 4L)
  #  id[[1]][wx]
  #}
  a1={
    lsp <- lapply(id, '[', x)
    lsf <- lapply(id, '[', y)
  },
  a0={
    wx <- which(x)
    wy <- which(y)
    lsp <- lapply(id, '[', wx)
    lsf <- lapply(id, '[', wy)
  },
  a2={
    wx <- which(x)
    lsp <- lapply(id, '[', wx)
    lsf <- lapply(id, '[', -wx)
  },
  a={
    lsp <- lapply(id, '[', wx)
    lsf <- lapply(id, '[', wy)
    # pxb <- lsp[[1]] + 1L
    # pyb <- lsp[[2]] + 2L
    # txb <- lsf[[1]] + 3L
    # tyb <- lsf[[2]] + 4L
  }
  # b={
  #   lsp2 <- lapply(id2, '[', x)
  #   lsf2 <- lapply(id2, '[', y);
  #   # pxb2 <- lsp2[[1]] + 1
  #   # pyb2 <- lsp2[[2]] + 2
  #   # txb2 <- lsf2[[1]] + 3
  #   # tyb2 <- lsf2[[2]] + 4
  # }
)
