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

# @param debug.lvl extract all triangles at that level, and only that level
#   (pass along a zero error matrix), ignored if zero

extract_mesh2 <- function(errors, tol, debug.lvl=0) {
  stopifnot(length(errors) > 1L)
  nr <- nrow(errors)
  nc <- ncol(errors)
  layers <- floor(min(log2(c(nr, nc))))
  undrawn <- array(TRUE, dim(errors))
  triangles <- vector('list', 2L * layers)

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    grid.nr <- ((nr - 1L) %/% mult)
    grid.nc <- ((nc - 1L) %/% mult)
    nr.g <- grid.nr * mult + 1L
    nc.g <- grid.nc * mult + 1L

    # - Vertical and Horizontal Bases ("diamond?") -

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

    ids.err <- errors[ids] > tol | (i  * 2 - 1)== debug.lvl
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

    ids.err <- errors[ids] > tol |  i * 2L == debug.lvl
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


# These shape offsets are not in use currently
# for each shape
# x/y offsets for each of the four child triangles, including NAs for missing
# ordered starting from breakpoint (hypotenuse midpoint) clockwise.  Shapes are
# in this order:
#
# 1. v.left
# 2. v.mid
# 3. v.right
# 4. h.top
# 5. h.mid
# 6. h.bot
# 7. tr->bl
# 8. tl->br

off.ex.tri <- array(
  c(
    0L,0L,   0L,1L,   1L,0L,    0L,0L,  1L,0L, 0L,-1L,  # vert left
    NA,NA,   NA,NA,   NA,NA,    NA,NA,  NA,NA, NA,NA,

    0L,0L,   0L,1L,   1L,0L,    0L,0L,  1L,0L, 0L,-1L,  # vert mid
    0L,0L,  0L,-1L,  -1L,0L,    0L,0L, -1L,0L, 0L, 1L,

    0L,0L,  0L,-1L,  -1L,0L,    0L,0L, -1L,0L, 0L,1L,   # vert right
    NA,NA,   NA,NA,   NA,NA,    NA,NA,  NA,NA, NA,NA,

    0L,0L,   1L,0L,  -1L,0L,    0L,0L, -1L,0L, -1L,0L,  # hrz top
    NA,NA,   NA,NA,   NA,NA,    NA,NA,  NA,NA, NA,NA,

    0L,0L,   1L,0L,  -1L,0L,    0L,0L, -1L,0L, -1L,0L,  # hrz mid
    0L,0L,  -1L,0L,   0L,1L,    0L,0L,  0L,1L,  1L,0L,

    0L,0L,  -1L,0L,   0L,1L,    0L,0L,  0L,1L,  1L,0L,  # hrz bot
    NA,NA,   NA,NA,   NA,NA,    NA,NA,  NA,NA,  NA,NA,

    0L,0L,  -1L,1L,   1L,1L,    0L,0L,   1L,1L, 1L,-1L,  # diag
    0L,0L,  1L,-1L, -1L,-1L,    0L,0L, -1L,-1L, -1L,1L,

    0L,0L,  -1L,1L,   1L,1L,    0L,0L,   1L,1L, 1L,-1L,  # diag (same)
    0L,0L,  1L,-1L, -1L,-1L,    0L,0L, -1L,-1L, -1L,1L
  ),
  dim=c(2L, 3L, 8L)
)
# Offsets to get children from parent hypotenuse midpoint
#
# Alternate x/y coords, permuted so that after collapsing x/y into linear ID we
# can select any of the eight types of offsets with the row selector

off.ex.mid <- aperm(
  array(
    c(
      1L,1L,   1L,-1L,  NA,NA,   NA,NA,    # vert left
      1L,1L,   1L,-1L, -1L,1L,  -1L,-1L,   # vert mid
      -1L,1L, -1L,-1L,  NA,NA,   NA,NA,    # vert right
      -1L,1L,  1L,1L,   NA,NA,   NA,NA,    # hrz top
       1L,1L,  1L,-1L, -1L,1L,  -1L,-1L,   # hrz mid
      1L,-1L, -1L,-1L,  NA,NA,   NA,NA,    # hrz bot
      0L,1L,   1L,0L,   0L,-1L, -1L,0L,    # diag
      0L,1L,   1L,0L,   0L,-1L, -1L,0L     # diag
    ),
    dim=c(2L, 4L, 8L),
  ),
  c(1L, 3L, 2L)
)

# Maybe we really want to keep coordinates in x/y as we're going to expand
# back to x/y here

extract_tris <- function(id, nr) {
  dx <- id[['x','tar']] - id[['x','par']]
  dy <- id[['y','tar']] - id[['y','par']]

  res.x <- rbind(id[['x','par']], id[['x','tar']] + dy, id[['x','tar']] - dy)
  res.y <- rbind(id[['y','par']], id[['y','tar']] - dx, id[['y','tar']] + dx)

  res.x * nr + res.y + 1L
}
next_children <- function(id) {
  dx <- (id[['x','tar']] - id[['x','par']])/2L
  dy <- (id[['y','tar']] - id[['y','par']])/2L
  dx_p_dy <- dx + dy
  dx_m_dy <- dx - dy

  id[,'par'] <- lapply(id[,'tar'], rep, 2L)
  id[,'tar'] <- list(
      c(id[['x','tar']] - dx_p_dy, id[['x','tar']] - dx_m_dy),
      c(id[['y','tar']] + dx_m_dy, id[['y','tar']] - dx_p_dy)
  )
  id
}
extract_mesh3 <- function(errors, tol) {
  nr <- nrow(errors)
  nc <- ncol(errors)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  id.dat <- id.pass <- id.fail <- matrix(
    list(
      rep(c(nc - 1L) %/% 2L, 2L), rep(c(nr - 1L) %/% 2L, 2L),
      c(0L, nc - 1L), c(nr - 1L, 0L)
    ),
    2L, 2L, dimnames=list(c('x', 'y'), c('tar', 'par'))
  )
  res <- vector('list', 2L * layers + 1L)
  for(i in seq_len(layers)) {
    mult <- as.integer(2^(layers - i))

    # diag first, then axis
    for(j in 1:2) {
      ids <- id.dat[['x','tar']] * nr + id.dat[['y','tar']] + 1L

      pass <- errors[ids] <= tol
      wpass <- which(pass)
      wfail <- which(!pass)
      id.pass[] <- lapply(id.dat, '[', wpass)
      id.fail[] <- lapply(id.dat, '[', wfail)

      res[[i * 2L - (j == 1L)]] <- extract_tris(id.pass, nr)

      id.dat <- next_children(id.fail)
  } }
  # Any remaining failures must be split to lowest level

  res[[2L * layers + 1L]] <- extract_tris(id.dat, nr)
  res
}
# Non square inputs
#
# Need to compute all the diagonal tiles that are added at a given size which
# will be either:
#
# 1. All the diagonals that fit at the start
# 2. Incremental diagonals fit in the margins between previous less granular
#    layers and the current layer
#
# For all of these we'll need the parents.
#
# For 1., need all midpoints within the current tile rows/cols.
# For 2.,
#
# * If next row/col can fit one more at more granular resolution, compute
#   midpoint for those.  If we do both then there is a duplicate midpoint; do we
#   want to handle that case explicitly?
# 
# One overall question

seed_ids <- function(offset, m, length, hrz) {
  mids <- seq(m, length.out=length, by=m * 2L)
  a <- rep(mids, 2L)
  b <- rep(m + offset, 2L * length)
  c <- c(mids - m, mids + m)
  d <- b + rep(c(m, -m, -m, m), length.out=length * 2L)

  matrix(
    if(hrz) list(a, b, c, d) else list(b, a, d, c),
    2L, 2L, dimnames=list(c('x', 'y'), c('tar', 'par'))
  )
}

extract_mesh3a <- function(errors, tol) {
  nr <- nrow(errors)
  nc <- ncol(errors)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  res <- vector('list', 2L * layers + 1L)
  id.dat <- id.pass <- id.fail <- seed_ids(0L, 0L, 0L, FALSE)
  trow <- tcol <- 0L

  for(i in seq_len(layers)) {
    # seed initial and additional triangles
    m <- as.integer(2^(layers - i))
    trow.p <- trow * 2L
    tcol.p <- tcol * 2L
    trow <- ((nr - 1L) %/% (m * 2L))
    tcol <- ((nc - 1L) %/% (m * 2L))
    row.ex <- trow.p < trow
    col.ex <- tcol.p < tcol
    seed.h <- seed.v <- replicate(4L, integer(), simplify=FALSE)
    if(i == 1L) {
      if(trow < tcol) seed.h <- seed_ids(0L, m, tcol, TRUE)
      else seed.v <- seed_ids(0L, m, trow, FALSE)
    } else {
      if(row.ex) seed.h <- seed_ids(trow.p * m * 2L, m, tcol.p + col.ex, TRUE)
      if(col.ex) seed.v <- seed_ids(tcol.p * m * 2L, m, trow.p, FALSE)
    }
    id.dat[] <- Map(c, id.dat, seed.h, seed.v)

    # diag first, then axis
    for(j in 1:2) {
      ids <- id.dat[['x','tar']] * nr + id.dat[['y','tar']] + 1L
      pass <- errors[ids] <= tol
      wpass <- which(pass)
      wfail <- which(!pass)

      id.pass[] <- lapply(id.dat, '[', wpass)
      id.fail[] <- lapply(id.dat, '[', wfail)

      res[[i * 2L - (j == 1L)]] <- extract_tris(id.pass, nr)

      id.dat <- next_children(id.fail)
  } }
  # Any remaining failures must be split to lowest level

  res[[2L * layers + 1L]] <- extract_tris(id.dat, nr)
  res
}


