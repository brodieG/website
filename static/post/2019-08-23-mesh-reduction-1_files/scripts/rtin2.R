errors_rtin2 <- function(terrain) {
  errors <- array(NA_real_, dim(terrain))
  numSmallestTriangles <- prod(dim(terrain) - 1L)
  numTriangles <- numSmallestTriangles * 2 - 2
  lastLevelIndex <-
    numTriangles - numSmallestTriangles
  gridSize <- nrow(terrain)
  tileSize <- gridSize - 1L

  # iterate over all possible triangles,
  # starting with smallest ones
  for (i in (numTriangles - 1):0) {
    id <- i + 2L
    mx<-my<-rcx<-rcy<-lcx<-lcy<-NA_real_
    ax<-ay<-bx<-by<-cx<-cy<-0L
    if (bitwAnd(id, 1L)) {
      # bottom-right triangle
      bx <- by <- cx <- tileSize
    } else {
      # top-left triangle
      ax <- ay <- cy <- tileSize
    }
    # Find small
    while ((id <- (id %/% 2L)) > 1L) {
      tmpx <- (ax + bx) / 2L
      tmpy <- (ay + by) / 2L

      if (bitwAnd(id, 1L)) {
        # right sub-triangle
        bx <- ax
        by <- ay
        ax <- cx
        ay <- cy
      } else {
        # left sub-triangle
        ax <- bx
        ay <- by
        bx <- cx
        by <- cy
      }
      cx <- tmpx
      cy <- tmpy
    }
    az <- terrain[ax + 1, ay + 1]
    bz <- terrain[bx + 1, by + 1]
    interpolatedHeight <- (az + bz) / 2

    # Error in hypothenuse midpoint
    mx <- ((ax + bx) / 2L)
    my <- ((ay + by) / 2L)
    mz <- terrain[mx + 1, my + 1L]
    middleError <- max(na.rm=TRUE,
      abs(interpolatedHeight - mz),
      errors[mx+1, my+1]
    )

    # Propagate left/right child errors
    lcError <- rcError <- 0
    if (i < lastLevelIndex) {
      lcx <- (ax + cx) / 2L
      lcy <- (ay + cy) / 2L
      lcError <- errors[lcx+1, lcy+1]
      rcx <- (bx + cx) / 2L
      rcy <- (by + cy) / 2L
      rcError <- errors[rcx+1, rcy+1]
    }
    errors[mx+1, my+1] <- max(
      middleError, lcError, rcError
    )
  }
  errors
}
