
# File originally from http://tylermw.com/data/dem_01.tif.zip
eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

# Compute the diagonals of the tiles.  There is one fewer tile than there are
# cols per row, and also than there are rows per col.

make_diags <- function(splits.x, splits.y, map, last.id=0, thresh=Inf) {
  x.raw <- .row(c(length(splits.x), length(splits.y)))
  y.raw <- .col(c(length(splits.x), length(splits.y)))
  within(
    list(), {
      col <- y.raw[-ncol(x.raw), -ncol(y.raw)]
      row <- x.raw[-nrow(x.raw), -ncol(x.raw)]
      x1 <- splits.x[c(row)]
      y1 <- splits.y[c(col)]
      x2 <- splits.x[c(x.raw[-1, -1])]
      y2 <- splits.y[c(y.raw[-1, -1])]
      z1 <- map[cbind(x1, y1)]
      z2 <- map[cbind(x2, y2)]
      mid.coord <- cbind((x1 + x2) / 2, (y1 + y2) / 2)
      zE <- (z1 + z2) / 2 - map[mid.coord]
} ) }
split_tile <- function(x1, x2, y1, y2, xu, yu) {
  # Organize our data into vertices of the tile going in clockwise order

  idx.base <- rep(1:4, each=2L)
  idx <- rep(5L, 12L)
  idx[-(seq_len(4) * 3L)] <- c(idx.base[-1L], idx.base[1L])
  vert.dat <- array(
    c(
      list(x1, x2, x2, x1, xu)[idx],
      list(y1, y1, y2, y2, yu)[idx]
    ),
    c(3, 4, 2)  # 3 verts/triangle, 4 triangles/tile, 2 coords/vert
  )
  # Append the triangles coords of each triangle to each other to
  # simplify the data structure

  array(
    lapply(asplit(vert.dat, c(1,3)), unlist),
    c(3, 2), dimnames=list(vertex=1:3, dim=c('x', 'y'))
  )
}
compute_error <- function(map) {
  x <- nrow(map)
  y <- ncol(map)

  layers <- floor(min(log2(c(x, y))))
  diags <- vector("list", layers)

  .get_err <- function(ids.mid) {
    ids.start <- ids.mid - (mult %/% 2L)
    ids.end <- ids.start + mult
    z <- map[ids.mid]
    z.mid <- (z[ids.start] + z[ids.end]) / 2L
    abs(z.mid - map[ids.mid])
  }
  .get_child_err <- function(ids.mid, type) {
    if(identical(type, 'square')) {
      col.off <- c(-1L, 1L, 1L, -1L)
      row.off <- c(-1L, -1L, 1L, 1L)
    } else if (identical(type, 'diamond')) {
      col.off <- c(0L, 1L, 0L, -1L)
      row.off <- c(-1L, 0L, 1L, 0L)
    } else stop("bad input")
    row.off <- row.off * mult %/% 2L
    col.off <- col.off * x * mult %/% 2L

    child.ids <- lapply(
      seq_along(row.off),
      function(i) ids.mid + (row.off[i] + col.off[i])
    )
    if(identical(type, 'square')) {
      # deal with oob children that may occur in square mode.  b/c of wrapping
      # indices this is a bit of a PITA.  Would be easier with matrix index.
      # Ids are expected to be in vertical then horizontal order.
      cells <- (dim.x - 1L) * (dim.y)
      col.1 <- seq_len(dim.x - 1L)
      col.n <- seq(cells - dim.x, cells, by=1L)
      row.1 <- seq(cells + 1L, 2 * cells - dim.x + 1L, by=dim.x)
      row.n <- seq(cells + dim.x - 1L, 2 * cells, by=dim.x)
      child.ids[[1]][c(row.1, col.1)] <- NA
      child.ids[[2]][c(row.1, col.n)] <- NA
      child.ids[[3]][c(row.n, col.n)] <- NA
      child.ids[[4]][c(row.n, col.1)] <- NA
    }
    lapply(child.ids, function(ids) errors[ids])
  }
  errors <- array(0, dim=dim(map))

  # at each interval range, we want to:
  # * compute the tiling in squares,
  # * then in diamonds
  # * compute errors at each of the midpoints of the size
  # * retrieve the four errors surrounding each midpoint

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    dim.x <- ((x - 1L) %/% mult) + 1L
    dim.y <- ((y - 1L) %/% mult) + 1L

    # square - start with vertical errors with a vector of the linear indices
    # into the error matrix.
    #
    # +-*-+   Stars are the points we want to compute the erors for, the long
    # |\ /|   edges of the triangles.  Children will be on the diagonals.
    # * X *
    # |/ \|
    # +-*-+

    ids.raw <- rep(mult, (dim.x - 1L) * (dim.y))
    ids.raw[1L] <- 1L
    ids.raw[seq(dim.x, length.out=dim.y - 1L, by=dim.x - 1L)] <-
      mult - 1L + (x - (dim.x - 1L) * mult + (x * mult %/% 2L))

    ids.v.start <- cumsum(ids.raw)
    ids.v.mid <- ids.v.start + mult %/% 2L
    ids.v.end <- ids.v.start + mult
    err.v <- abs(map[ids.v.mid] - (map[ids.v.start] + map[ids.v.end]) / 2)

    ids.h.start <- c(matrix(ids.v.start, ncol=dim.x - 1L, byrow=TRUE))
    ids.h.mid <- ids.h.start + (x * mult %/% 2L)
    ids.h.end <- ids.h.start + (x * mult)
    err.h <- abs(map[ids.h.mid] - (map[ids.h.start] + map[ids.h.end]) / 2)

    ids.mid <- c(ids.v.mid, ids.h.mid)
    z.err <- c(err.v, err.h)

    errors.child <- .get_child_err(ids.mid, 'square')
    errors[ids.mid] <- do.call(pmax, c(list(z.err), errors.child))

    # Diamonds - sides are diagonals: imagine rotating the previous pictogram 45
    # degrees.  One big thing though, we don't actually do the diamonds, we
    # decompose them into the diagonals in the grid, which are their sides.

    ids.tl <- ids.v.start[seq_len((dim.x - 2L) * (dim.y - 1L))]
    ids.br <- ids.tl + (x * mult) + mult %/% 2L
    ids.mid.tl <- (ids.tl + ids.br) %/% 2L
    z.mid <- (map[ids.tl] + map[ids.br]) / 2L
    err.tl <- abs(map[ids.mid.tl] - z.mid)

    ids.tr <- ids.tl + x
    ids.bl <- ids.tl + mult
    z.mid <- (map[ids.tr] + map[ids.bl]) / 2L
    ids.mid.tr <- (ids.tr + ids.bl) %/% 2L
    err.tr <- abs(map[ids.mid.tr] - z.mid)

    ids.mid <- c(ids.mid.tl, ids.mid.tr)
    errors.child <- .get_child_err(ids.mid, 'diamond')
    errors[ids.mid] <- do.call(pmax, c(list(z.err, errors.child)))
  }
  errors
}
errors <- compute_error(map)
err.ind <- which(errors > 20, arr.ind=TRUE)


compute_errors2 <- function(map) {

}

# For each error, we need to draw all the corresponding triangles.  This means
# we need to figure out the size of the "diagonal", and whether we're dealing
# with a diamond or square.


# we need to compute even level odd level tiles.  For odd tiles (1 index), we
# check the actual diagonal, i.e. tile is square.  For even tiles, we treat the
# tile as a rhombus so the diagonal is actually parallel to the x axis (or y
# axis depending on how you draw it)?

# Try a direct implementation

compute_errors <- function(terrain) {
  errors <- array(0, dim(terrain));
  numSmallestTriangles = prod(dim(terrain));
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
    }
    # calculate error in the middle of the long edge of the triangle

    interpolatedHeight = (
      terrain[ay * gridSize + ax + 1L] + terrain[by * gridSize + bx + 1L]
    ) / 2;
    middleIndex =
      bitwShiftR(ay + by, 1L) * gridSize + bitwShiftR(ax + bx, 1L) + 1L;
    middleError = abs(interpolatedHeight - terrain[middleIndex]);

    if (i >= lastLevelIndex) { # smallest triangles
      errors[middleIndex] = middleError;
    } else { # bigger triangles; accumulate error with children
      leftChildError = errors[
        bitwShiftR(ay + cy, 1L) * gridSize + bitwShiftR(ax + cx, 1L) + 1L
      ];
      rightChildError = errors[
        bitwShiftR(by + cy, 1L) * gridSize + bitwShiftR(bx + cx, 1L) + 1L
      ];
      errors[middleIndex] = max(
        c(errors[middleIndex], middleError, leftChildError, rightChildError)
      );
    }
  }
  errors;
}
map <- elmat1[1:257,1:257]
system.time(errors <- compute_errors(map))
treeprof(errors <- compute_errors(map))

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
system.time({
  raw <- extract_geometry(errors, diff(range(map)) / 200)
  xs <- matrix(raw %/% nrow(errors), 3)
  ys <- matrix(raw %% nrow(errors), 3)
})
plot_new(xs, ys)
polygon(
  rescale(rbind(xs, NA)), rescale(rbind(ys, NA)),
  col='#DDDDDD', border='#444444'
)


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


mx <- matrix(0, 1e3, 1e3)
ids <- seq_len(nrow(mx) / 2L)

system.time({
  seq.row <- seq(1L, length.out=500L, by=2L)
  seq.col <- seq(0L, to=1000000L, by=1000L)
  ids <- rep(seq.row, length(seq.col)) + rep(seq.col, each=length(seq.row))
})

system.time({
  ids2 <- rep(2L, 5e5)
  ids2[seq_len(500) * 500L] <- 500L
  ids2 <- cumsum(ids2)
})

