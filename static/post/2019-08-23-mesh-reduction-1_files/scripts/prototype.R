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
# tol <- diff(range(map)) / 50
tol <- diff(range(map))
# debug(extract_mesh2)
system.time(tris <- extract_mesh2(errors, tol))
# treeprof::treeprof((tris <- extract_mesh2(errors, tol))
plot_tri_ids(tris, dim(errors))
plot_points_ids(which(errors > tol), dim(map))


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




## Messing With Watching Algo

map <- elmat1[1:3, 1:3]
