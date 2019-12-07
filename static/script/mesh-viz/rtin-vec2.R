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
) / 2 # have to do this for cleanliness of code, not to expensive to convert

xx <- do.call(rbind, lapply(asplit(offset.dg,3), as.data.frame))
xx[['V3']] <- rep(letters[1:5], each=8)
ggplot(xx) + geom_point(aes(V1, V2, color=V3, shape=V3), size=2, alpha=.5)

xx <- do.call(rbind, lapply(asplit(offset.ax,3), as.data.frame))
xx[['V3']] <- rep(letters[1:5], each=4)
ggplot(xx) + geom_point(aes(V1, V2, color=V3), alpha=.5)

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

# There are two nasty things about this implementation:
#
# * highly repetitive data (including generation of child points for
#   first level.
# * Need to sort
#
# Ticks: 1423; Iterations: 103; Time Per: 52.48 milliseconds; Time Total: 5.405 seconds; Time Ticks: 1.423
# 
#                           milliseconds
# compute_error3 ------- : 52.48 -  0.00
#     compute_os ------- : 24.75 -  0.22
#     |   co3 ---------- : 24.16 -  1.29
#     |   |   matrix --- : 22.87 - 22.83
#     |   co2 ---------- :  0.30 -  0.26
#     order ------------ : 20.58 - 20.32
#     array ------------ :  3.91 -  3.91
#     do.call ---------- :  2.21 -  0.00
#     |   <Anonymous> -- :  2.21 -  2.14
#     diff ------------- :  0.55 -  0.18
#     - ---------------- :  0.44 -  0.44
# 
# Maybe we can reduce timings by 1/3 by removing the children for the first pass
# and it might even be possible to remove the duplicate parent calc (although
# that seems a lot more complicated)

compute_error3 <- function(map) {
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
      o <- if(j == 'diag') offset.dg else offset.ax
      if(j == 'diag' && i == layers) o <- o[1:2,,]
      odim <- dim(o)
      o <- as.integer(o * mult %/% 2L)
      dim(o) <- odim
      # {
      #   if(tile.nr < 2L) o <- o[c(1L,2L,5L,6L),,]
      #   else tile.nr <- tile.nr %/% 2L
      #   if(tile.nc < 2L) o <- o[seq_len(dim(o)[1L]/2L),,]
      #   else tile.nc <- tile.nc %/% 2L
      # }
      onr <- diff(range(o[,1,]))
      onc <- diff(range(o[,2,]))
      ctimes <- tile.nc / onc
      rtimes <- tile.nr / onr

      # o1 <- c(o[,1L,] + o[,2L,] * nr + 1L)
      # o2 <- o1 + rep((seq_len(ctimes) - 1L) * onr, each=length(o1))
      # o3 <- o2 + rep((seq_len(rtimes) - 1L) * nr * onc, each=length(o2))

      o3 <- compute_os(o, nr, ctimes, rtimes, onr, onc)
      reps <- ctimes * rtimes
      # array(o3, c(odim[1],5,reps))
      # array(seq_along(o3), c(odim[1],5,reps))
      oid <- seq_len(odim[1]) +
        rep((seq_len(reps) - 1) * length(o3)/reps, each=odim[1])

      err.list <- vector('list', if(i < 2L) 2L else 4L)
      err.list[[1L]] <- abs(
        map[o3[oid]] - (map[o3[oid + odim[1]]] + map[o3[oid + odim[1] * 2L]])/2
      )
      err.list[[2L]] <- errors[o3[oid]]
      if(i >= 2L || j == 'diag') {
        err.list[[3L]] <- errors[o3[oid + odim[1] * 3L]]
        err.list[[4L]] <- errors[o3[oid + odim[1] * 4L]]
      }
      err.vals <- do.call(pmax, err.list)
      err.ord <- order(err.vals)
      errors[o3[oid][err.ord]] <- err.vals[err.ord]
      # errors[o3[oid]] <- err.vals
  } }
  errors
}
