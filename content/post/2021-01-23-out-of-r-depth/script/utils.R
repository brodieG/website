
find_water <- function(map) {
  stopifnot(length(map) > 0)
  map.rle <- rle(sort(c(map)))
  water.level <- map.rle$values[which.max(map.rle$lengths)]
  # Asssume no water on edges of map
  water <- map == water.level
  water[c(1,nrow(water)),] <- FALSE
  water[,c(1,ncol(water))] <- FALSE
  water
}
find_shore <- function(water) {
  stopifnot(length(water) > 0)
  neighbors <- as.matrix(subset(expand.grid(x=-1:1, y=-1:1), x | y))
  water.i <- which(water, arr.ind=TRUE)
  shore <- array(FALSE, dim(water))
  for(i in seq_len(nrow(neighbors))) {
    shore[
      cbind(
        water.i[, 1] + neighbors[i, 1],
        water.i[, 2] + neighbors[i, 2]
      )
    ] <- TRUE
  }
  shore <- shore & !water
}
# Deduplicate two col matrices
#
# ~10x faster than duplicated.

dedupi <- function(x) {
  if(nrow(x)) {
    len <- nrow(x)
    o <- order(x[,1], x[,2])
    x <- x[o,, drop=FALSE]
    tmp <- abs(x[-1,,drop=FALSE] - x[-len,,drop=FALSE])
    c(TRUE, tmp[,1] | tmp[,2])[order(o)]
  } else logical()
}
# this could be made faster as we don't need the double order
# (i.e. don't use dedupi which has to re-order back)
dedup <- function(x) x[dedupi(x),,drop=FALSE]

