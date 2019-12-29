# qq <- data.frame(
#   id=o3[c(oid, oid+cycle, oid+2*cycle)], type=rep(c('a','b','c'), each=length(oid))
# )
# qq <- transform(qq, x=((id - 1) %/% 5), y=((id -1) %% 5))
# ggplot(qq) +
#   geom_point(aes(x,y,color=type, shape=type), size=3, alpha=.5) + 
#   facet_wrap(~type)


# Offsets are: parenta, parentb, h midpoint, children with variable number of
# children supported

offset.dg <- aperm(
  array(
    c(
      0L,0L, 2L,2L, 1L,1L, 1L,2L, 0L,1L, 1L,0L, 2L,1L,
      4L,0L, 2L,2L, 3L,1L, 3L,2L, 4L,1L, 2L,1L, 3L,0L,
      2L,2L, 0L,4L, 1L,3L, 1L,4L, 2L,3L, 0L,3L, 1L,2L,
      2L,2L, 4L,4L, 3L,3L, 3L,4L, 2L,3L, 3L,2L, 4L,3L
    ),
    dim=c(2L, 7L, 4L)
  ),
  c(3L, 1L, 2L)
)
# Smallest size that allows us to define the children with integer
# values.
offset.ax <- aperm(
  array(
    c(
      0L,0L, 4L,0L, 2L,0L, 3L,1L, 1L,1L,
      4L,0L, 4L,4L, 4L,2L, 3L,3L, 3L,1L,
      4L,4L, 0L,4L, 2L,4L, 1L,3L, 3L,3L,
      0L,4L, 0L,0L, 0L,2L, 1L,1L, 1L,3L
    ),
    dim=c(2L, 5L, 4L)
  ),
  c(3L, 1L, 2L)
)


# xx <- do.call(rbind, lapply(asplit(offset.dg,3), as.data.frame))
# xx[['V3']] <- rep(letters[1:5], each=8)
# ggplot(xx) + geom_point(aes(V1, V2, color=V3, shape=V3), size=2, alpha=.5)
#
# xx <- do.call(rbind, lapply(asplit(offset.ax,3), as.data.frame))
# xx[['V3']] <- rep(letters[1:5], each=4)
# ggplot(xx) + geom_point(aes(V1, V2, color=V3), alpha=.5)

# split this out to get better sense of timings

co1 <- function(o, nr) c(o[,1L,] + o[,2L,] * nr + 1L)
co2 <- function(o1, ctimes, onr)
  o1 + rep((seq_len(ctimes) - 1L) * onr, each=length(o1))
co3 <- function(o2, rtimes, nr, onc)
  o2 + matrix(
    (seq_len(rtimes) - 1L) * nr * onc, nrow=length(o2), ncol=rtimes,
    byrow=TRUE
  )


compute_os <- function(o, nr, ctimes, rtimes, onr, onc) {
  o1 <- co1(o, nr)
  o2 <- co2(o1, ctimes, onr)
  o3 <- co3(o2, rtimes, nr, onc)
  o3
}
co12 <- function(o, nr) c(o[,1L,] + o[,2L,] * nr + 1L)
co22 <- function(o1, ctimes, onr)
  matrix(o1, ctimes, length(o1), byrow=TRUE) +
    (seq_len(ctimes) - 1L) * onr
co32 <- function(o2, rtimes, nr, onc)
  matrix(o2, rtimes, length(o2), byrow=TRUE) +
    (seq_len(rtimes) - 1L) * nr * onc

compute_os2 <- function(o, nr, ctimes, rtimes, onr, onc) {
  o1 <- co12(o, nr)
  o2 <- co22(o1, ctimes, onr)
  o3 <- co32(o2, rtimes, nr, onc)
  o3
}

# There are two nasty things about this implementation:
#
# * highly repetitive data for square case
# * Need to sort
#
# Currently this is not working with new order of offsets. 
#
# Ticks: 3736; Iterations: 162; Time Per: 34.21 milliseconds; Time Total: 5.542 seconds; Time Ticks: 3.736
# 
#                           milliseconds
# compute_error3 ------- : 34.21 -  0.00
#     .get_errs -------- : 22.39 - 22.37
#     compute_os ------- :  5.82 -  0.05
#     |   co3 ---------- :  5.59 -  0.43
#     |       matrix --- :  5.15 -  5.15
#     matrix ----------- :  2.07 -  2.07
#     order ------------ :  1.53 -  1.51
#     array ------------ :  1.28 -  1.28
#     do.call ---------- :  0.93 -  0.03
#     |   <Anonymous> -- :  0.91 -  0.84
#     diff ------------- :  0.18 -  0.06
#
# > microbenchmark::microbenchmark(compute_error3(map), compute_error(map), f(map, 257L))
# Unit: milliseconds
#                 expr       min       lq      mean  median        uq      max
#  compute_error3(map) 19.010240 20.83107 22.871070 21.2681 22.494988 31.01237
#   compute_error(map) 12.522519 13.32934 15.493201 13.8249 14.690842 24.31099
#         f(map, 257L)  6.055984  6.69855  7.047833  6.8176  7.017205 16.43509
#  neval
#    100
#    100
#    100


compute_error3 <- function(map) {
  if(!all(dim(map) %% 2L) || min(dim(map)) <= 2L) stop("invalid map")
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  errors <- array(0, dim=dim(map))

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    tile.nr <- ((nr - 1L) %/% mult) * mult
    tile.nc <- ((nc - 1L) %/% mult) * mult

    for(j in c('axis','diag')) {
      axis <- j == 'axis'
      o <- if(axis) offset.ax else offset.dg
      if(!axis && i == layers) o <- o[1L,,,drop=FALSE]
      if(axis  && i == 1L) o <- o[,,1:3,drop=FALSE]

      od <- dim(o)
      o <- (o * mult) %/% (if(axis) 4L else 2L)
      onr <- diff(range(o[,1,]))
      onc <- diff(range(o[,2,]))
      ctimes <- tile.nc / onc
      rtimes <- tile.nr / onr
      o <- c(o[,1L,] + o[,2L,] * nr + 1L)
      o <- matrix(o, ctimes, length(o), byrow=TRUE) +
        (seq_len(ctimes) - 1L) * onr
      o <- matrix(o, rtimes, length(o), byrow=TRUE) +
        (seq_len(rtimes) - 1L) * nr * onc

      oid <- seq_len(od[1L] * ctimes * rtimes)
      cycle <- length(oid)
      err.list <- vector('list', od[3L] - 2L)
      err.list[[1L]] <- abs(
        map[o[oid]] - (map[o[oid + cycle]] + map[o[oid + 2L * cycle]])/2
      )
      for(k in seq_len(length(err.list[-1L])))
        err.list[[k + 1L]] <- errors[o[oid + cycle * (k + 2L)]]

      err.vals <- do.call(pmax, err.list)
      if(axis && i > 1L) {
        err.ord <- order(err.vals)
        errors[o[oid][err.ord]] <- err.vals[err.ord]
      } else {
        errors[o[oid]] <- err.vals
      }
  } }
  errors
}

init_offsets <- function(i, j, mult, layers) {
  axis <- j == 'axis'
  o.raw <- if(axis) offset.ax else offset.dg
  if(!axis && i == layers)
    o.raw <- o.raw[1L,,,drop=F]
  if(axis && i == 1L)
    o.raw <- o.raw[,,1:3,drop=F]
  o.raw <- (o.raw * mult) %/% (if(axis) 4L else 2L)
  o.raw
}

# Version of compute_error3 clarified for watching purposes

compute_error3b <- function(map) {
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  errors <- array(NA_real_, dim=dim(map))

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    tile.nr <- ((nr - 1L) %/% mult) * mult
    tile.nc <- ((nc - 1L) %/% mult) * mult

    for(j in c('axis', 'diag')) {
      err.ids <- NULL
      o <- o.m <- o.a <- o.b <- err.val <- NA

      o.raw <- init_offsets(i, j, mult, layers)
      o.nr <- diff(range(o.raw[,1,]))
      o.nc <- diff(range(o.raw[,2,]))
      o.dim <- dim(o.raw)
      err.n <- o.dim[3L] - 2L

      c.rep <- tile.nc / o.nc
      r.rep <- tile.nr / o.nr
      o <- c(o.raw[,1L,] + o.raw[,2L,] * nr + 1L)
      o <- rep_each(o, c.rep) +
        (seq_len(c.rep) - 1L) * o.nr
      o <- rep_each(o, r.rep) +
        (seq_len(r.rep) - 1L) * nr * o.nc

      oid <- seq_len(o.dim[1L] * c.rep * r.rep)
      o.len <- length(oid)
      o.a <- o[oid]
      o.b <- o[oid + o.len]
      o.m <- o[oid + 2 * o.len]

      err.ids <- err.vals <- vector('list', err.n)
      err.ids[[1L]] <- o.m

      m.est <- (map[o.a] + map[o.b]) / 2
      errors[o.m] <- abs(map[o.m] - m.est)

      for(k in seq_len(err.n)) {
        err.ids[[k]] <- o[oid + o.len * (k + 1L)]
        err.vals[[k]] <- errors[err.ids[[k]]]
      }
      err.val <- do.call(pmax, err.vals)
      err.ord <- order(err.val)
      errors[o.m[err.ord]] <- err.val[err.ord]
  } }
  errors
}
