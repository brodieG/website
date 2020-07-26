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
# Calculate 3D Vector Cross Product
#
# "Vectorized" by accepting 3 row matrices for inputs.  You're responsible to
# check that the dimensions make sense.
#
# @param a 3xN matrix
# @param b 3xN matrix

xprod <- function(a, b) {
  if(!is.matrix(a)) a <- matrix(a)
  if(!is.matrix(b)) b <- matrix(b)
  rbind(
     (a[2,] * b[3,] - a[3,] * b[2,]),
    -(a[1,] * b[3,] - a[3,] * b[1,]),
     (a[1,] * b[2,] - a[2,] * b[1,])
  )
}
# Barycentric coordinates

bary_M <- function(p, v) {
  det <- (v[2,2]-v[3,2])*(v[1,1]-v[3,1]) +
         (v[3,1]-v[2,1])*(v[1,2]-v[3,2])

  l1 <- (
          (v[2,2]-v[3,2]) * (p[,1]-v[3,1]) +
          (v[3,1]-v[2,1]) * (p[,2]-v[3,2])
        ) / det
  l2 <- (
          (v[3,2]-v[1,2]) * (p[,1]-v[3,1]) +
          (v[1,1]-v[3,1]) * (p[,2]-v[3,2])
        ) / det
  l3 <- 1 - l1 - l2
  cbind(l1, l2, l3)
}
