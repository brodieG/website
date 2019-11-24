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

