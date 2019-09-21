
# File originally from http://tylermw.com/data/dem_01.tif.zip
eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))


# retrieve children around ids.mid clockwise starting from top left
get_child_ids <- function(ids.mid, type, nr, nc, mult) {
  if(identical(type, 's')) {          # square sides
    col.off <- c(-1L, 1L, 1L, -1L)
    row.off <- c(-1L, -1L, 1L, 1L)
  } else if (identical(type, 'd')) {  # diagonals
    col.off <- c(0L, 2L, 0L, -2L)
    row.off <- c(-2L, 0L, 2L, 0L)
  } else stop("bad input")
  offset <- (row.off * mult) %/% 4L + nr * (col.off * mult) %/% 4L
  child.ids <- lapply(seq_along(offset), function(i) ids.mid + offset[i])
  if(identical(type, 's')) {
    # square sides on perimeter will produce some OOB children depending on
    # which child it is (remember, children clockwise from top left)
    ids.mod.x <- ids.mid %% nr
    ids.div.y <- (ids.mid - 1L) %/% nc
    row.1 <- ids.mod.x == 1L
    row.n <- ids.mod.x == 0L
    col.1 <- ids.div.y == 0L
    col.n <- ids.div.y == (nc - 1L)
    child.ids[[1]][row.1 | col.1] <- NA
    child.ids[[2]][row.1 | col.n] <- NA
    child.ids[[3]][row.n | col.n] <- NA
    child.ids[[4]][row.n | col.1] <- NA
  }
  child.ids
}
compute_error <- function(map) {
  .pmax2 <- function(a, b) do.call(pmax, c(list(a, na.rm=TRUE), b))
  .get_child_err <- function(ids.mid, type) {
    child.ids <- get_child_ids(ids.mid, type, nr, nc, mult)
    lapply(child.ids, function(ids) errors[ids])
  }
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc))))
  errors <- array(0, dim=dim(map))

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    grid.nr <- ((nr - 1L) %/% mult) + 1L
    grid.nc <- ((nc - 1L) %/% mult) + 1L
    ids.raw <- rep(mult, prod(grid.nr, grid.nc))
    ids.raw[1L] <- 1L
    ids.raw[seq_len(grid.nc - 1L) * grid.nr + 1L] <-
      nr * (mult - 1L) + ((nr - 1L) - (grid.nr - 1L) * mult) + 1L
    ids.raw <- matrix(cumsum(ids.raw), grid.nr, grid.nc)

    # - Square, vertical
    ids.a.start <- c(ids.raw[-grid.nr,])
    ids.a.mid <- ids.a.start + mult %/% 2L
    ids.a.end <- ids.a.start + mult
    err.a <- abs(map[ids.a.mid] - (map[ids.a.start] + map[ids.a.end]) / 2)

    # - Square, horizontal
    ids.b.start <- c(t(ids.raw[,-grid.nc]))
    ids.b.mid <- ids.b.start + (mult %/% 2L) * nr
    ids.b.end <- ids.b.start + (mult * nr)
    err.b <- abs(map[ids.b.mid] - (map[ids.b.start] + map[ids.b.end]) / 2)

    # - Square record errors
    ids.mid <- c(ids.a.mid, ids.b.mid)
    z.err <- c(err.a, err.b)
    errors[ids.mid] <- if(i > 1L) pmax2(z.err, .get_child_err(ids.mid, 's'))
    else z.err

    # - Diagonal: TL to BR
    ids.a.start <- c(ids.raw[seq(1L, grid.nr-1L, 2L), seq(1L, grid.nc-1L, 2L)])
    ids.a.off <- mult * (nr + 1L)
    if(grid.nr > 2L & grid.nc > 2L)
      ids.a.start <- c(ids.a.start, ids.a.start + ids.a.off)
    ids.a.end <- ids.a.start + ids.a.off
    ids.a.mid <- (ids.a.start + ids.a.end - 1L) %/% 2L + 1L
    z.mid <- (map[ids.a.start] + map[ids.a.end]) / 2L
    err.a <- abs(map[ids.a.mid] - z.mid)

    # - Diagonal: TR to BL
    ids.b.start <- c(
      if(grid.nc>2L) c(ids.raw[seq(2L, grid.nr, 2L), seq(2L, grid.nc-1L, 2L)]),
      if(grid.nr>2L) c(ids.raw[seq(3L, grid.nr, 2L), seq(1L, grid.nc-1L, 2L)])
    )
    ids.b.off <- mult * (nr - 1L)
    ids.b.end <- ids.b.start + ids.b.off
    z.mid <- (map[ids.b.start] + map[ids.b.end]) / 2L
    ids.b.mid <- (ids.b.start + ids.b.end - 1L) %/% 2L + 1L
    err.b <- abs(map[ids.b.mid] - z.mid)

    # - Diagonal record errors
    ids.mid <- c(ids.a.mid, ids.b.mid)
    errors[ids.mid] <- pmax2(c(err.a, err.b), .get_child_err(ids.mid, 'd'))
  }
  errors
}
map <- elmat1[1:9,1:20]
map <- elmat1[1:257,1:257]
# debug(compute_error)
errors <- compute_error(map)

system.time(errors <- compute_error(elmat1[1:257,1:257]))
treeprof::treeprof(errors <- compute_error(elmat1[1:257,1:257]))
errors <- compute_error(volcano[1:61,1:61])
err.ind <- which(errors > 20, arr.ind=TRUE)
# 1. start from the lowest level out
# 2. check diag point error, if fail:
# 3. get child diag coordinates
# 4. if NA, already plotted, don't plot
# 5. else, generate child triangles
# 6. mark diag point as NA
# 7. at top level, draw remaining children

# Alternative (more like original)
#
# 1. Start from top level
#    * Get initial ids, in theory spreading out from those should guarantee
#    getting all the midpoints.
# 2. If not failing, draw the two triangles
#    * need to know if vert/hz tr/tl
# 3. If failing, get children
# 4. repeat


x.square <- as.integer(c(0, 0, 1, 0, 1, 0, 0,  0, -1, 0, -1, 0))
y.square <- as.integer(c(0, 1, 0, 0, 0,-1, 0, -1,  0, 0,  0, 1))
x.diag <- as.integer(c(0, -1, 1, 0,  1, 1, 0,  1, -1, 0, -1,-1))
y.diag <- as.integer(c(0,  1, 1, 0,  1,-1, 0, -1, -1, 0, -1, 1))

# gah, the problem with this is that when we get to the first level that didn't
# fail, we need to draw the triangles with the smallest split possible since
# that level did not fail.  So we need to be able to tell easily whether it is a
# vert/hrz or tl/tr tile.
#
# Formula for vert/hrz:
#
# * if x coord a multiple of `mult`, then vertical, else horizontal
#
# Formula for tl/tr
#
# * if both x and y coord shifted by mult/2 and divided `mult` are even or both
#   are odd, then tl, else br

draw_triangle2 <- function(ids.mid, type, nr, nc, mult) {
  ids.y <- ((ids.mid - 1L) %% nr)
  ids.x <- ((ids.mid - 1L) %/% nc)
  x.off <- if(identical(type, 's')) x.square else x.diag
  y.off <- if(identical(type, 's')) y.square else y.diag
  list(
    x=outer(x.off * (mult / 2L), ids.x, '+'),
    y=outer(y.off * (mult / 2L), ids.y, '+')
  )
}
draw_triangle <- function(ids.mid, type, nr, nc, mult) {
  ids.y <- ((ids.mid - 1L) %% nr)
  ids.x <- ((ids.mid - 1L) %/% nc)

  if(identical(type,  's')) {
    off.a <- c( 0L,   0L, -1L,  0L,  0L, 1L) * (mult/2L)
    off.b <- c(-1L,   1L,  0L,  1L, -1L, 0L) * (mult/2L)

    vertical <- which(as.logical((ids.x - 1L) %% mult))
    x.res <- c(
      outer(off.a, ids.x[vertical],  '+'),
      outer(off.b, ids.x[-vertical], '+')
    )
    y.res <- c(
      outer(off.b, ids.y[vertical],  '+'),
      outer(off.a, ids.y[-vertical], '+')
    )
  } else if(identical(type, 'd')) {
    off.a <- c(-1L,  1L,  1L,  1L, -1L, -1L) * mult / 2
    off.b <- c( 1L,  1L, -1L, -1L, -1L,  1L) * mult / 2

    off.aa <- c(-1L,  1L, -1L, -1L,  1L,  1L) * mult / 2
    off.bb <- c( 1L,  1L, -1L, -1L,  1L, -1L) * mult / 2

    col.odd <- (ids.y - mult %/% 2L) %/% mult %% 2L
    row.odd <- (ids.x - mult %/% 2L) %/% mult %% 2L

    # top left diags are those for which both x and y grid coords are
    # each even or odd, whereas top right ones are of mixed evenness.  Because
    # our coords are in original ids computing whether we are in an even or odd
    # grid grouping gets complicated.

    topright <- which((col.odd &row.odd) | (!col.odd & !row.odd))

    x.res <- c(
      outer(off.a, ids.x[-topright],  '+'),
      outer(off.aa, ids.x[topright], '+')
    )
    y.res <- c(
      outer(off.b, ids.y[-topright],  '+'),
      outer(off.bb, ids.y[topright], '+')
    )
  }
  list(x=x.res, y=y.res)
}
# Triangle Base Coords
#
# Returns for each id, the four pairs of ids of the base vertices connected
# to that ids.  E.g, for `ids.mid` x, will return a,b,c,d, with first the
# two ids for a, then b, etc.
#
#              a---b                  a
#              |\ /|                 /|\
#  type = 'd': | x |    type = 's': d-x-b
#              |/ \|                 \|/
#              c---d                  c

base_coords <- function(ids.mid, type, nr, nc, mult) {
  if(identical(type, 's')) {
    r.off <- c(+0L, +1L, +1L, +0L, +0L, -1L, -1L, +0L)
    c.off <- c(+1L, +0L, +0L, -1L, -1L, +0L, +0L, +1L)
  } else if(identical(type, 'd')) {
    r.off <- c(-1L, +1L, +1L, +1L, +1L, -1L, -1L, -1L)
    c.off <- c(+1L, +1L, +1L, -1L, -1L, -1L, -1L, +1L)
  }
  off <- r.off * mult %/%2L + c.off * (nr * mult %/%2L)
  rep(ids.mid, each=length(off)) + rep(off, length(ids.mid))
}
# Alternate extract mesh algo:
#
# 1. Compute long edge midpoints at lowest level
# 2. If error > tolerance:
# 3.   Find the four triangles that have that midpoint for a vertex
# 4.   Check which of those have undrawn vertices
# 5.   Draw them
# 6.   Mark drawn vertices as drawn
# 7. Increase level
# 8. Compute long edge midpoints
# 9. Go to 2.

extract_mesh2 <- function(errors, tol) {
  stopifnot(length(errors) > 1L)
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc))))
  undrawn <- array(TRUE, dim(errors))
  triangles <- vector('list', 2L * layers)

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    grid.nr <- ((nr - 1L) %/% mult) + 1L
    grid.nc <- ((nc - 1L) %/% mult) + 1L
    r.extra <- nr - ((grid.nr - 1L) * mult + 1L)

    # - Vertical and Horizontal Bases ("square") -

    # Index munging to compute midpoints for long edges that are vertical
    # or horizontal

    c.lens <- rep_len(c(grid.nr %/% 2L, grid.nr %/% 2L + 1L), grid.nc + 1L)
    ids.len <- sum(c.lens)
    ids.r.raw <- rep_len(
      c(seq(mult %/% 2L + 1L, nr, by=mult), seq(1L, nr, by=mult)),
      length.out=ids.len
    )
    ids.c.raw <- (rep(seq_len(grid.nc + 1L), c.lens) - 1L) * mult %/% 2L * nr
    ids <- ids.c.raw + ids.r.raw

    # Midpoints at edges of tile will produce oob triangles with our naive
    # offset algorithm.  To detect the oob triangles we must add padding to our
    # indices as otherwise oob offsets would overlap with valid offsets.

    ids.pad <- ids + ids.c.raw + nr

    # Which midpoints exeede tolerance

    ids.err <- errors[ids] > tol
    ids.pad.err <- ids.pad[ids.err]

    # Coords for the bases for each of four triangles that form the tile around
    # each midpoint.

    base.vert <- base_coords(ids.pad.err, type='s', nr * 2L, nc, mult)

    # detect oob, and then undo padding

    base.vert.inb <- base.vert > 0L & base.vert <= (nr * nc * 2L) &
      (((base.vert - 1L) %/% nr) %% 2L) == 1L
    base.vert <- base.vert - ((base.vert - 1L) %/% (nr * 2L) + 1L) * nr

    # detect whether a vertex has already been drawn
    # UPDATE (for first pass we should draw all, no need to check)

    base.vert.undrawn <- rep(TRUE, length(base.vert))
    base.vert.undrawn[base.vert.inb] <- undrawn[base.vert[base.vert.inb]]

    # valid triangles are those that have no oob vertices and have at least one
    # of two vertices not drawn.

    tri.draw <-
      .colSums(base.vert.undrawn, m=2L, n=length(base.vert.undrawn)/2L) &
      .colSums(base.vert.inb, m=2L, n=length(base.vert.inb)/2L) == 2L

    # generate triangle coords, and mark the vertices as drawn.

    tri.draw.vert <- matrix(base.vert[rep(tri.draw, each=2L)], nrow=2)
    triangles[[(i - 1L) * 2L + 1L]] <-
      rbind(tri.draw.vert, rep(ids[ids.err], each=4L)[tri.draw])
    undrawn[c(tri.draw.vert)] <- FALSE

    # - Diagonal Bases -

    ids.raw <- rep(mult, (grid.nr - 1L) * (grid.nc - 1L))
    ids.raw[seq(1L, by=grid.nr - 1L, length.out=grid.nc - 1L)] <-
      2L * mult - 1L + nr * (mult %/% 2L) + r.extra
    ids.raw[1] <- ids.raw[1] - r.extra - mult + 1L
    ids <- cumsum(ids.raw)
    ids.err <- errors[ids] > tol

    base.vert <- base_coords(ids[ids.err], type='d', nr, nc, mult)
    base.vert.undrawn <- undrawn[base.vert]
    tri.draw <-
      .colSums(base.vert.undrawn, m=2L, n=length(base.vert.undrawn)/2L) > 0

    tri.draw.vert <- matrix(base.vert[rep(tri.draw, each=2L)], nrow=2)
    triangles[[i * 2L]] <-
      rbind(tri.draw.vert, rep(ids[ids.err], each=4L)[tri.draw])
    undrawn[c(tri.draw.vert)] <- FALSE
  }
  triangles
}
debug(extract_mesh2)
tris <- extract_mesh2(errors, tol)
plot_tri_ids(tris, dim(errors))

extract_mesh <- function(errors, tol) {
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc))))

  # Need to find the midpoints of the largest k^n + 1 squares that fit into
  # our grid

  mult <- 2^layers
  points.r <- ((nr - 1L) %/% mult)
  points.c <- ((nc - 1L) %/% mult)

  # if we don't use ids elsewhere we need to direct compute the indices
  ids <- array(seq_along(errors), dim(errors))
  points <- ids[
    seq(1L + mult %/% 2L, length.out = points.r, by=mult),
    seq(1L + mult %/% 2L, length.out = points.c, by=mult)
  ]
  triangles <- vector("list", (layers + 1L) * 2L)

  # gaaah, it matters what the failing parent of the non failing child is, we
  # only want to draw the triangle that touches the failing parent

  for(i in seq_len(layers)) {
    mult <- as.integer(2^(layers - i + 1L))

    err.p <- errors[points] > tol
    triangles[[2L * (i - 1L) + 1L]] <-
      draw_triangle(points[!err.p], 'd', nr, nc, mult)
    points <- unique(unlist(get_child_ids(points[err.p], 'd', nr, nc, mult)))

    err.p <- errors[points] > tol
    triangles[[2L * i]] <- draw_triangle(points[!err.p], 's', nr, nc, mult)
    points <- unique(unlist(get_child_ids(points[err.p], 's', nr, nc, mult)))
  }
  list(tri=triangles, points=points)
}
# debug(extract_mesh)
tol <- 1
map <- elmat1[1:5, 1:5]
# map <- elmat1[1:17, 1:17]
# map <- elmat1[1:257, 1:257]
errors <- compute_error(map)
xx <- extract_mesh(errors, tol)
x0 <- matrix(unlist(lapply(xx$tri, '[[', 'x')), 3)
y0 <- matrix(unlist(lapply(xx$tri, '[[', 'y')), 3)
valid <- which(colSums(
  x0 < 0 | y0 < 0 | x0 > nrow(map) - 1 | y0 > ncol(map) - 1) == 0
)
x <- rbind(x0[, valid], NA)
y <- rbind(y0[, valid], NA)
plot_new(x, y)
polygon(rescale(x), rescale(y), col='#DDDDDD', border='#444444')

plot_triangles <- function(tri, nr, nc) {
  x0 <- matrix(tri$x, 3)
  y0 <- matrix(tri$y, 3)
  valid <- which(colSums(
    x0 < 0 | y0 < 0 | x0 > nr - 1 | y0 > nc - 1) == 0
  )
  x <- rbind(x0[, valid], NA)
  y <- rbind(y0[, valid], NA)
  plot_new(x, y)
  polygon(rescale(x), rescale(y), col='#DDDDDD', border='#444444')
}
plot_tri_ids <- function(tri, dim) {
  ids <- rbind(do.call(cbind, tri), NA) - 1L
  x <- ids %% dim[1]
  y <- ids %/% dim[2]
  plot_new(x, y)
  polygon(rescale(x), rescale(y), col='#DDDDDD', border='#444444')
}

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
    while ((id <- bitwShiftR(id, 1L)) > 1L) {
      mx = bitwShiftR(ax + bx, 1L)
      my = bitwShiftR(ay + by, 1L)

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
    middleIndex =
      bitwShiftR(ax + bx, 1L) * gridSize + bitwShiftR(ay + by, 1L) + 1L;
    middleError = abs(interpolatedHeight - terrain[middleIndex]);

    if (i >= lastLevelIndex) { # smallest triangles
      errors[middleIndex] = middleError;
    } else { # bigger triangles; accumulate error with children
      leftChildError = errors[
        bitwShiftR(ax + cx, 1L) * gridSize + bitwShiftR(ay + cy, 1L) + 1L
      ];
      rightChildError = errors[
        bitwShiftR(bx + cx, 1L) * gridSize + bitwShiftR(by + cy, 1L) + 1L
      ];
      errors[middleIndex] = max(
        c(errors[middleIndex], middleError, leftChildError, rightChildError)
      );
    }
  }
  errors;
}
map <- elmat1[1:5, 1:5]
map <- elmat1[1:17, 1:17]
map <- elmat1[1:257, 1:257]
system.time(errors2 <- errors_rtin(map))
errors <- compute_error(map)

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
raw <- extract_geometry(errors, tol)
xs <- matrix(raw %/% nrow(errors), 3)
ys <- matrix(raw %% nrow(errors), 3)
plot_new(xs, ys)
polygon(
  rescale(rbind(xs, NA)), rescale(rbind(ys, NA)),
  col='#DDDDDD', border='#444444'
)
points(rescale(which(errors > tol, arr.ind=TRUE)[,2:1] - 1), pch=19, col='red')


# map <- elmat1
map <- volcano
system.time(xx <- compute_layers(map, thresh=1))
plot_mesh(xx, facet=FALSE)
# old.par <- par(mfrow=c(3,2), mar=numeric(4))
# lapply(xx, function(x) {
#   res <- x[['draw']]
#   dim(res) <- dim(x[['id']])
#   plot(as.raster(res))
# })

plot_mesh <- function(tiles, facet=FALSE) {
  dev.off()
  dev.new()
  if(facet) {
    rows <- floor(sqrt(length(xx)))
    cols <- ceiling(length(xx) / rows)
    old.par <- par(mfrow=c(rows, cols), mar=numeric(4))
  } else {
    par(mai=numeric(4))
    plot_new(0, 1)
    rect(0, 0, 1, 1, col='green', border='green')
  }
  lapply(
    rev(tiles),
    function(x) {
      if(facet) {
        plot_new(0, 1)
        rect(0, 0, 1, 1, col='green', border='green')
      }
      x.new <- lapply(x, '[', x[['draw']])
      with(x.new,
        rect(
          (x1 - 1) / (nrow(map) - 1),
          (y2 - 1) / (ncol(map) - 1),
          (x2 - 1) / (nrow(map) - 1),
          (y1 - 1) / (ncol(map) - 1),
          col = '#FFFFFF66',
          # col = gray((zu - min(map))/diff(range(map))),
          border='black'
          # border = gray((zu - min(map))/diff(range(map)))
          # col=ifelse(draw, gray((zu - min(map))/diff(range(map))), 'green'),
          # border=ifelse(draw, gray((zu - min(map))/diff(range(map))), 'green')
        )
      )
  })
}


## Rescale data to a range from 0 to `range` where `range` in (0,1]
rescale <- function(x, range=1, center=0.5)
  ((x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))) * range +
   (1 - range) * center

## Prepare a plot with a particular aspect ratio
plot_new <- function(
  x, y, xlim=c(0,1), ylim=c(0,1),
  par.args=list(mai=numeric(4L), xaxt='n', yaxt='n', xaxs='i', yaxs='i')
) {
  if(length(par.args)) do.call(par, par.args)
  plot.new()
  plot.window(
    xlim, ylim, asp=diff(range(y, na.rm=TRUE))/diff(range(x, na.rm=TRUE))
) }

