errors_rtin2 <- function(terrain) {
  errors <- array(NA_real_, dim(terrain))
  nSmallestTriangles <- prod(dim(terrain) - 1L)
  nTriangles <- nSmallestTriangles * 2 - 2
  lastLevelIndex <-
    nTriangles - nSmallestTriangles
  gridSize <- nrow(terrain)
  tileSize <- gridSize - 1L

  # iterate over all possible triangles,
  # starting with smallest ones
  for (i in (nTriangles - 1):0) {
    id <- i + 2L
    mx <- my <- rcx <- rcy <- lcx <- lcy <-
      ax <- ay <- bx <- by <- cx <- cy <- NA
    if (bitwAnd(id, 1L)) {
      # bottom-right triangle
      bx <- by <- cx <- tileSize
      ax <- ay <- cy <- 0
    } else {
      # top-left triangle
      ax <- ay <- cy <- tileSize
      bx <- by <- cx <- 0
    }
    # Find small
    while ((id <- (id %/% 2)) > 1L) {
      tmpx <- (ax + bx) / 2
      tmpy <- (ay + by) / 2

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
    mx <- ((ax + bx) / 2)
    my <- ((ay + by) / 2)
    mz <- terrain[mx + 1, my + 1]
    middleError <- max(na.rm=TRUE,
      abs(interpolatedHeight - mz),
      errors[mx+1, my+1]
    )
    errors[mx+1, my+1] <- middleError

    # Propagate left/right child errors
    lcError <- rcError <- 0
    if (i < lastLevelIndex) {
      lcx <- (ax + cx) / 2
      lcy <- (ay + cy) / 2
      lcError <- errors[lcx+1, lcy+1]
      rcx <- (bx + cx) / 2
      rcy <- (by + cy) / 2
      rcError <- errors[rcx+1, rcy+1]
    }
    errors[mx+1, my+1] <- max(
      errors[mx+1, my+1], lcError, rcError
    )
  }
  errors
}
