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
    ids.div.y <- (ids.mid - 1L) %/% nr
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
  if(!all(dim(map) %% 2L) || min(dim(map)) <= 2L) stop("invalid map")
  .pmax2 <- function(a, b) do.call(pmax, c(list(a, na.rm=TRUE), b))
  .get_child_err <- function(ids.mid, type) {
    child.ids <- get_child_ids(ids.mid, type, nr, nc, mult)
    lapply(child.ids, function(ids) errors[ids])
  }
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  errors <- array(0, dim=dim(map))

  # Force tile splits in areas that don't fall in 2^layers squares

  if((r.extra <- (nr - 1L) %% 2L^layers)) {
    while(r.extra - 2^(floor(log2(r.extra))))
      r.extra <- r.extra - 2^(floor(log2(r.extra)))
    errors[nr, seq(r.extra / 2L + 1L, nc, by=r.extra)] <- Inf
  }
  if((c.extra <- (nc - 1L) %% 2L^layers)) {
    while(c.extra - 2^(floor(log2(c.extra))))
      c.extra <- c.extra - 2^(floor(log2(c.extra)))
    errors[seq(c.extra / 2L + 1L, nr, by=c.extra), nc] <- Inf
  }
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
    z.err <- pmax(c(err.a, err.b), errors[ids.mid])
    errors[ids.mid] <-
      if(i > 1L) .pmax2(z.err, .get_child_err(ids.mid, 's'))
      else pmax(z.err, errors[ids.mid])

    # - Diagonal: TL to BR
    ids.a.raw <- ids.raw[
      seq(1L, grid.nr - 1L, 2L), seq(1L, grid.nc - 1L, 2L), drop=FALSE
    ]
    ids.a.off <- mult * (nr + 1L)
    ids.a.start <- c(
      ids.a.raw,
      ids.a.raw[
        if(grid.nr %% 2L) TRUE else -nrow(ids.a.raw),
        if(grid.nc %% 2L) TRUE else -ncol(ids.a.raw)
      ] + ids.a.off
    )
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
    z.err <- pmax(c(err.a, err.b), errors[ids.mid])
    errors[ids.mid] <- .pmax2(z.err, .get_child_err(ids.mid, 'd'))
  }
  errors
}
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

# problem right now is that when we add the fake errors to cause the partial
# strips to be drawn our logic only works if we have adjscent size strips
# happening, if we skip one size then there is no conveyance of the break, the
# break needs to carry all the way.

extract_mesh2 <- function(errors, tol) {
  stopifnot(length(errors) > 1L)
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc))))
  undrawn <- array(TRUE, dim(errors))
  triangles <- vector('list', 2L * layers)

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    grid.nr <- ((nr - 1L) %/% mult)
    grid.nc <- ((nc - 1L) %/% mult)
    nr.g <- grid.nr * mult + 1L
    nc.g <- grid.nc * mult + 1L

    # - Vertical and Horizontal Bases ("square") -

    # Index munging to compute midpoints for long edges that are vertical
    # or horizontal

    c.lens <- rep_len(c(grid.nr, grid.nr + 1L), grid.nc * 2L + 1L)
    ids.len <- sum(c.lens)
    ids.r.raw <- rep_len(
      c(seq(mult %/% 2L + 1L, nr.g, by=mult), seq(1L, nr.g, by=mult)),
      length.out=ids.len
    )
    ids.c.raw <- (rep(seq_len(grid.nc * 2L + 1L), c.lens) - 1L) *
      mult %/% 2L * nr
    ids <- ids.c.raw + ids.r.raw

    # Midpoints at edges of tile will produce oob triangles with our naive
    # offset algorithm.  To detect the oob triangles we must add padding to our
    # indices as otherwise oob offsets would overlap with valid offsets.

    ids.pad <- ids + ids.c.raw + nr

    # Error, or at edge of plot and we will see no more larger triangles when
    # plot is strictly square and of 2^k + 1.

    ids.err <- errors[ids] > tol
    ids.pad.err <- ids.pad[ids.err]

    # Coords for the bases for each of four triangles that form the tile around
    # each midpoint.

    base.vert <- base_coords(ids.pad.err, type='s', nr * 2L, nc, mult)

    # detect oob, and then undo padding

    base.vert.inb <- base.vert > 0L & base.vert <= (nr * nc * 2L) &
      (((base.vert - 1L) %/% nr) %% 2L) == 1L
    base.vert <- base.vert - ((base.vert - 1L) %/% (nr * 2L) + 1L) * nr

    # detect whether a triangle has already been drawn

    tri.inb <- .colSums(base.vert.inb, m=2L, n=length(base.vert.inb)/2L) == 2L
    base.vert.undrawn <- rep(TRUE, length(base.vert)/2L)
    base.vert.mid.id <-
      .colMeans(base.vert[rep(tri.inb, each=2L)], m=2L, n=sum(tri.inb))
    base.vert.undrawn[tri.inb] <- undrawn[base.vert.mid.id]

    # valid triangles are those that have no oob vertices and have at least one
    # of two vertices not drawn.

    tri.draw <- base.vert.undrawn & tri.inb

    # generate triangle coords, and mark the vertices as drawn.

    tri.draw.vert <- matrix(base.vert[rep(tri.draw, each=2L)], nrow=2)
    triangles[[(i - 1L) * 2L + 1L]] <-
      rbind(tri.draw.vert, rep(ids[ids.err], each=4L)[tri.draw])
    undrawn[ids[ids.err]] <- FALSE

    # - Diagonal Bases -

    ids.r.raw <- seq(mult %/% 2L + 1L, length.out=grid.nr, by=mult)
    ids.c.raw <- seq(nr * mult %/% 2L, length.out=grid.nc, by=nr * mult)
    ids <- rep(ids.r.raw, each=length(ids.c.raw)) +
      rep(ids.c.raw, length(ids.r.raw))

    ids.err <- errors[ids] > tol
    base.vert <- base_coords(ids[ids.err], type='d', nr, nc, mult)
    base.vert.mid.id <- .colMeans(base.vert, m=2L, n=length(base.vert)/2L)
    base.vert.undrawn <- undrawn[base.vert.mid.id]
    tri.draw <- base.vert.undrawn

    tri.draw.vert <- matrix(base.vert[rep(tri.draw, each=2L)], nrow=2)
    triangles[[i * 2L]] <-
      rbind(tri.draw.vert, rep(ids[ids.err], each=4L)[tri.draw])
    undrawn[ids[ids.err]] <- FALSE

    # Need to draw any undrawn triangles at this particular level (or maybe
    # pair of levels?)
  }
  triangles
}
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
# map <- elmat1[1:257,1:257]
system.time(errors <- compute_error(map))
# tol <- diff(range(map)) / 50
tol <- diff(range(map))
# debug(extract_mesh2)
system.time(tris <- extract_mesh2(errors, tol))
# treeprof::treeprof((tris <- extract_mesh2(errors, tol))
plot_tri_ids(tris, dim(errors))
plot_points_ids(which(errors > tol), dim(map))

plot_tri_ids <- function(tri, dim, new=TRUE) {
  ids <- rbind(do.call(cbind, tri), NA) - 1L
  x <- ids %% dim[1]
  y <- ids %/% dim[1]
  if(new) plot_new(x, y)
  polygon(x/(dim[1] - 1), y/(dim[2]-1), col='#DDDDDD', border='#444444')
}
plot_points_ids <- function(points.ids, dim, cex=1, col='red') {
  ids <- points.ids - 1L
  x <- ids %% dim[1]
  y <- ids %/% dim[1]
  x0 <- seq_len(dim[1]) - 1L
  y0 <- seq_len(dim[2]) - 1L
  points(
    rep(x0/max(x0), each=dim[2]), rep(y0/max(y0), dim[1]),
    pch=16, col='black', cex=0.5
  )
  points(x/max(x0), y/max(y0), pch=16, col=col, cex=cex)
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
xs <- matrix(raw %% nrow(errors), 3)
ys <- matrix(raw %/% ncol(errors), 3)
plot_new(xs, ys)
polygon(
  rescale(rbind(xs, NA)), rescale(rbind(ys, NA)),
  col='#DDDDDD', border='#444444'
)
points(rescale(which(errors > tol, arr.ind=TRUE)[,2:1] - 1), pch=19, col='red')



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

