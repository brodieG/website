# Offsets are: h midpoint, parenta, parentb, children with variable number of
# children supported

offset.dg <- aperm(
  array(
    c(
      1L,1L, 0L,0L, 2L,2L, 1L,2L, 0L,1L,
      1L,1L, 0L,0L, 2L,2L, 1L,0L, 2L,1L,
      3L,1L, 4L,0L, 2L,2L, 3L,2L, 4L,1L,
      3L,1L, 4L,0L, 2L,2L, 2L,1L, 3L,0L,
      1L,3L, 2L,2L, 0L,4L, 1L,4L, 2L,3L,
      1L,3L, 2L,2L, 0L,4L, 0L,3L, 1L,2L,
      3L,3L, 2L,2L, 4L,4L, 3L,4L, 2L,3L,
      3L,3L, 2L,2L, 4L,4L, 3L,2L, 4L,3L
    ),
    dim=c(2L, 5L, 8L)
  ),
  c(3L, 1L, 2L)
)
offset.dg <- aperm(
  array(
    c(
      1L,1L, 0L,0L, 2L,2L, 1L,2L, 0L,1L, 1L,0L, 2L,1L,
      3L,1L, 4L,0L, 2L,2L, 3L,2L, 4L,1L, 2L,1L, 3L,0L,
      1L,3L, 2L,2L, 0L,4L, 1L,4L, 2L,3L, 0L,3L, 1L,2L,
      3L,3L, 2L,2L, 4L,4L, 3L,4L, 2L,3L, 3L,2L, 4L,3L
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
      2L,0L, 0L,0L, 4L,0L, 3L,1L, 1L,1L,
      4L,2L, 4L,0L, 4L,4L, 3L,3L, 3L,1L,
      2L,4L, 4L,4L, 0L,4L, 1L,3L, 3L,3L,
      0L,2L, 0L,4L, 0L,0L, 1L,1L, 1L,3L
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

compute_error3 <- function(map) {
  .get_errs <- function(o3, oid, od, reps) {
    cycle <- oid[1] * reps
    err.list <- vector('list', od[3L] - 2L)
    err.list[[1L]] <- abs(
      map[o3[oid]] - (map[o3[oid + cycle]] + map[o3[oid + 2L * cycle]])/2
    )
    for(k in seq_len(length(err.list[-1L])))
      err.list[[k + 1L]] <- errors[o3[oid + cycle * (k + 2L)]]
    err.list
  }
  if(!all(dim(map) %% 2L) || min(dim(map)) <= 2L) stop("invalid map")
  # offsets are row/col, start at parent and go clockwise, offsets are
  # already multiplied by 2L b/c otherwise we would have fractional offsets
  # for smallest square set.
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  errors <- array(0, dim=dim(map))

  # # Force tile splits in areas that don't fall in 2^layers squares

  # if((r.extra <- (nr - 1L) %% 2L^layers)) {
  #   while(r.extra - 2^(floor(log2(r.extra))))
  #     r.extra <- r.extra - 2^(floor(log2(r.extra)))
  #   errors[nr, seq(r.extra / 2L + 1L, nc, by=r.extra)] <- Inf
  # }
  # if((c.extra <- (nc - 1L) %% 2L^layers)) {
  #   while(c.extra - 2^(floor(log2(c.extra))))
  #     c.extra <- c.extra - 2^(floor(log2(c.extra)))
  #   errors[seq(c.extra / 2L + 1L, nr, by=c.extra), nc] <- Inf
  # }

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

      # o1 <- c(o[,1L,] + o[,2L,] * nr + 1L)
      # o2 <- o1 + rep((seq_len(ctimes) - 1L) * onr, each=length(o1))
      # o3 <- o2 + rep((seq_len(rtimes) - 1L) * nr * onc, each=length(o2))

      o3 <- compute_os2(o, nr, ctimes, rtimes, onr, onc)
      reps <- ctimes * rtimes
      # array(o3, c(od[1],od[3],reps))
      # array(seq_along(o3), c(od[1],5,reps))
      oid <- seq_len(od[1L] * reps)

      # err.list <- vector('list', od[3L] - 2L)
      # err.list[[1L]] <- abs(
      #   map[o3[oid]] - (map[o3[oid + od[1]]] + map[o3[oid + od[1] * 2L]])/2
      # )
      # for(k in seq_len(length(err.list[-1L])))
      #   err.list[[k + 1L]] <- errors[o3[oid + od[1] * (k + 2L)]]
      err.list <- .get_errs(o3, oid, od, reps)

      err.vals <- do.call(pmax, err.list)
      if(axis && i > 1L) {
        err.ord <- order(err.vals)
        errors[o3[oid][err.ord]] <- err.vals[err.ord]
      } else {
        errors[o3[oid]] <- err.vals
      }
  } }
  errors
}
