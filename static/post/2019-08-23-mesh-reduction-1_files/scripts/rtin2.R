# Cleaned up implementation for animation

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

# Try a direct implementation; mostly the same except that we need to transpose
# some of the coordinates

errors_rtin <- function(terrain) {
  errors <- array(0, dim(terrain));
  numSmallestTriangles = prod(dim(terrain) - 1L);
  numTriangles = numSmallestTriangles * 2 - 2;
  lastLevelIndex = numTriangles - numSmallestTriangles;
  gridSize = nrow(terrain)
  tileSize = gridSize - 1L

  # iterate over all possible triangles, starting from the smallest level
  for (i in (numTriangles - 1):0) {

    # get triangle coordinates from its index in an implicit binary tree
    id = i + 2L;
    ax = ay = bx = by = cx = cy = 0L;
    if (bitwAnd(id, 1L)) {
      bx = by = cx = tileSize; # bottom-left triangle
    } else {
      ax = ay = cy = tileSize; # top-right triangle
    }
    while ((id <- (id %/% 2L)) > 1L) {
      mx = (ax + bx) / 2L
      my = (ay + by) / 2L

      if (bitwAnd(id, 1L)) { # left half
        bx = ax; by = ay;
        ax = cx; ay = cy;
      } else {        # right half
        ax = bx; ay = by;
        bx = cx; by = cy;
      }
      cx = mx; cy = my;
      # polygon(c(ax, bx, cx)/4, c(ay, by, cy)/4, col='#00000003')
    }
    # calculate error in the middle of the long edge of the triangle

    interpolatedHeight = (
      terrain[ax * gridSize + ay + 1L] + terrain[bx * gridSize + by + 1L]
    ) / 2;
    middleIndex = ((ax + bx) / 2L) * gridSize + ((ay + by) / 2L) + 1L;
    middleError = abs(interpolatedHeight - terrain[middleIndex]);

    if (i >= lastLevelIndex) { # smallest triangles
      errors[middleIndex] = middleError;
    } else { # bigger triangles; accumulate error with children
      leftChildError = errors[
        ((ax + cx) / 2L) * gridSize + ((ay + cy) / 2L) + 1L
      ];
      rightChildError = errors[
        ((bx + cx) / 2L) * gridSize + ((by + cy) / 2L) + 1L
      ];
      errors[middleIndex] = max(
        c(errors[middleIndex], middleError, leftChildError, rightChildError)
      );
    }
  }
  errors;
}

extract_geometry <- function(errors, maxError) {
  i = 0;
  indices = integer(prod(dim(errors) - 1L))  # overallocate to max num triangles
  gridSize = nrow(errors)
  tileSize = gridSize - 1L

  processTriangle <- function(ax, ay, bx, by, cx, cy) {
    # middle of the long edge
    mx = (ax + bx) %/% 2L;
    my = (ay + by) %/% 2L;

    if (
      abs(ax - cx) + abs(ay - cy) > 1 &&
      errors[my * gridSize + mx + 1L] > maxError
    ) {
      # triangle doesn't approximate the surface well enough; split it into two
      processTriangle(cx, cy, ax, ay, mx, my);
      processTriangle(bx, by, cx, cy, mx, my);

    } else {
      ## add a triangle to the final mesh (note this is +1 the original)
      indices[(i <<- i + 1)] <<- ay * gridSize + ax;
      indices[(i <<- i + 1)] <<- by * gridSize + bx;
      indices[(i <<- i + 1)] <<- cy * gridSize + cx;
    }
  }
  processTriangle(0, 0, tileSize, tileSize, tileSize, 0);
  processTriangle(tileSize, tileSize, 0, 0, 0, tileSize);

  indices[seq_len(i)];
}
