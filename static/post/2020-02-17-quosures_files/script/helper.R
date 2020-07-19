# - Helpers  -------------------------------------------------------------------

library(vetr)
back_hex <- function(hex, obs, depth) {
  vetr::vetr(
    structure(list(numeric(7), numeric(7)), class='data.frame'),
  )
  hex.in <- rbind(hex[[1]], 0, hex[[2]])
  vecs <- hex.in - obs
  vecs.n <- vecs / abs(vecs[2]) * depth  # normalize to distances along Z
  hex.out <- hex.in + vecs.n
  data.frame(x=hex.out[1,], y=hex.out[3,], z=hex.out[2,])
}
# @param targets in 0-1 what fraction of distance along points to generate
#   interpolated coordinates.  Ideally angle changes between segments in points
#   are limited as otherwise the distance will be underestimated.

interp_along <- function(points, targets) {
  vetr::vetr(
    matrix(numeric(),nrow=3) && ncol(.) > 1,
    numeric() && all_bw(., 0, 1)
  )
  ds <- points[,-1] - points[,-ncol(points)]
  dsc <- cumsum(c(0, sqrt(colSums(ds^2))))
  dsc <- dsc/max(dsc)

  interval <- findInterval(targets, dsc, rightmost.closed=TRUE)
  low <- dsc[interval]
  high <- dsc[interval + 1]
  interp <- (targets - low) / (high - low)
  points[,interval] + ds[,interval] * rep(interp, each=3)
}
# @param a 3d vector
# @param b 3d vector

xprod <- function(a, b) {
  c(
     (a[2] * b[3] - a[3] * b[2]),
    -(a[1] * b[3] - a[3] * b[1]),
     (a[1] * b[2] - a[2] * b[1])
  )
}
