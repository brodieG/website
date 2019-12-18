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
# Get the start/end coords of hypotenuse, and then hypotenuse
#
# We need start/end so that we can compute the interpolated Z value to compute
# error against.  Otherwise we could completely focus on the hypotenuses.  But
# there is a pattern still.  For square we alternate each "row", for diamond we
# alternate each column.

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

    # - Diamond, vertical
    ids.a.start <- c(ids.raw[-grid.nr,])
    ids.a.mid <- ids.a.start + mult %/% 2L
    ids.a.end <- ids.a.start + mult
    err.a <- abs(map[ids.a.mid] - (map[ids.a.start] + map[ids.a.end]) / 2)

    # - Diamond, horizontal
    ids.b.start <- c(t(ids.raw[,-grid.nc]))
    ids.b.mid <- ids.b.start + (mult %/% 2L) * nr
    ids.b.end <- ids.b.start + (mult * nr)
    err.b <- abs(map[ids.b.mid] - (map[ids.b.start] + map[ids.b.end]) / 2)

    # - Diamond record errors
    ids.mid <- c(ids.a.mid, ids.b.mid)
    z.err <- pmax(c(err.a, err.b), errors[ids.mid])
    errors[ids.mid] <-
      if(i > 1L) .pmax2(z.err, .get_child_err(ids.mid, 's'))
      else pmax(z.err, errors[ids.mid])

    # - Square (Diagonal): TL to BR
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

    # - Square (Diagonal): TR to BL
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
# This version tries to work directly from the hypotenuses and find the `a/b`
# points off of it rather than the other way around.
#
# The order of the index types in the `ids` list is semantic!  Don't change it.

compute_error2 <- function(map) {
  # - Helper Funs --------------------------------------------------------------
  .get_child_err <- function(ids.mid, which, type) {
    if(identical(type, 'axis')) {          # square sides
      col.off <- c(-1L, 1L, 1L, -1L)
      row.off <- c(-1L, -1L, 1L, 1L)
    } else if (identical(type, 'diag')) {  # diagonals
      col.off <- c(0L, 2L, 0L, -2L)
      row.off <- c(-2L, 0L, 2L, 0L)
    } else stop("bad input")
    offset <- (row.off * mult) %/% 4L + nr * (col.off * mult) %/% 4L
    lapply(
      seq_along(offset)[which], function(i) errors[ids.mid + offset[i]]
  ) }
  .get_par_err <- function(ids, offsets)
    abs(map[ids] - (map[ids + offsets[1L]] + map[ids + offsets[2L]]) / 2)

  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  errors <- array(0, dim=dim(map))

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    mhalf <- mult %/% 2L
    tile.r <- ((nr - 1L) %/% mult)
    tile.c <- ((nc - 1L) %/% mult)

    # - Axis (vertical/horizontal) ---------------------------------------------

    # Compute IDs, distinguish b/w inside vs. outside
    col.vn <- tile.c + 1L
    col.hn <- tile.c
    col.v <- seq(1L, length.out=col.vn, by=mult)
    col.h <- seq(2L, length.out=tile.c, by=mult)
    ids.v <- seq(mhalf + 1L, nr, mult)
    ids.h <- seq(nr * mhalf + 1L, nr * (mhalf + 1L), by=mult)
    v.len <- length(ids.v)
    h.len <- length(ids.h)
    ids <- list(
      v.out.l=ids.v,
      v.in=ids.v + c(
        matrix((col.v[-c(1L,col.vn)] - 1L) * nr, v.len, col.vn - 2L, byrow=TRUE)
      ),
      v.out.r=ids.v + (col.v[col.vn] - 1L) * nr,
      h.out.t=ids.h[1L] + (seq_len(col.hn) - 1L) * nr * mult,
      h.in=ids.h[-c(1L, h.len)] + c(
        matrix((seq_len(col.hn)-1L) * nr * mult, h.len - 2L, col.hn, byrow=TRUE)
      ),
      h.out.b=ids.h[h.len] + (seq_len(col.hn) - 1L) * nr * mult
    )
    # Errors
    err.child <- if(i > 1L) {
      which <- list(2:3, 1:4, c(1L,4L), 3:4, 1:4, 1:2)
      Map(.get_child_err, ids, which, 'axis')
    }
    err.par <- c(
      Map(.get_par_err, ids[1:3], list(c(-mhalf, mhalf))),
      Map(.get_par_err, ids[4:6], list(c(-mhalf * nr, mhalf * nr)))
    )
    for(j in seq_along(ids)) {
      errors[ids[[j]]] <- do.call(
        pmax, c(list(na.rm=TRUE), err.par[j], err.child[[j]])
    ) }
    # - Diagonals --------------------------------------------------------------

    ids.raw <- seq(mhalf + nr * mhalf + 1L, nr * (mhalf + 1L), by=mult) +
      matrix((seq_len(tile.c) - 1L) * mult * nr, tile.r, tile.c, byrow=TRUE)
    which.sw <- xor((col(ids.raw) %% 2L), (row(ids.raw) %% 2L))
    ids <- list(nw=ids.raw[!which.sw], sw=ids.raw[which.sw])

    err.child <- Map(.get_child_err, ids, list(1:4), 'diag')
    err.par <- Map(
      .get_par_err, ids,
      list((mhalf * nr + mhalf) * c(1L, -1L), (mhalf * nr - mhalf) * c(1L, -1L))
    )
    for(j in seq_along(ids)) {
      errors[ids[[j]]] <- do.call(
        pmax, c(list(na.rm=TRUE), err.par[j], err.child[[j]])
    ) }
  }
  errors
}
# Works for non-square grids (when we're done with it)

compute_error2a <- function(map) {
  # - Helper Funs --------------------------------------------------------------
  .get_child_err <- function(ids.mid, which, type) {
    if(identical(type, 'axis')) {          # square sides
      col.off <- c(-1L, 1L, 1L, -1L)
      row.off <- c(-1L, -1L, 1L, 1L)
    } else if (identical(type, 'diag')) {  # diagonals
      col.off <- c(0L, 2L, 0L, -2L)
      row.off <- c(-2L, 0L, 2L, 0L)
    } else stop("bad input")
    offset <- (row.off * mult) %/% 4L + nr * (col.off * mult) %/% 4L
    lapply(
      seq_along(offset)[which], function(i) errors[ids.mid + offset[i]]
  ) }
  .get_par_err <- function(ids, offsets)
    abs(map[ids] - (map[ids + offsets[1L]] + map[ids + offsets[2L]]) / 2)

  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  errors <- array(0, dim=dim(map))

  # Need to add errors at the smallest seam between differnt size adjoining
  # tiles to make sure the larger ones are broken up.  What's here currently is
  # too aggressive

  if((r.extra <- (nr - 1L) %% 2L^layers)) {
    while(r.extra - 2^(floor(log2(r.extra))))
      r.extra <- r.extra - 2^(floor(log2(r.extra)))
    errors[nr - r.extra, seq(r.extra + 1L, nc - 1L, by=r.extra)] <- Inf
  }
  if((c.extra <- (nc - 1L) %% 2L^layers)) {
    while(c.extra - 2^(floor(log2(c.extra))))
      c.extra <- c.extra - 2^(floor(log2(c.extra)))
    errors[seq(c.extra + 1L, nr - 1L, by=c.extra), nc - c.extra] <- Inf
  }

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    mhalf <- mult %/% 2L
    tile.r <- ((nr - 1L) %/% mult)
    tile.c <- ((nc - 1L) %/% mult)

    # - Axis (vertical/horizontal) ---------------------------------------------

    # Compute IDs, distinguish b/w inside vs. outside
    col.vn <- tile.c + 1L
    col.hn <- tile.c
    col.v <- seq(1L, length.out=col.vn, by=mult)
    col.h <- seq(2L, length.out=tile.c, by=mult)
    ids.v <- seq(mhalf + 1L, nr, mult)
    ids.h <- seq(nr * mhalf + 1L, nr * (mhalf + 1L), by=mult)
    v.len <- length(ids.v)
    h.len <- length(ids.h)
    ids <- list(
      v.out.l=ids.v,
      v.in=ids.v + rep_each((col.v[-c(1L,col.vn)] - 1L) * nr, v.len),
      v.out.r=ids.v + (col.v[col.vn] - 1L) * nr,
      h.out.t=ids.h[1L] + (seq_len(col.hn) - 1L) * nr * mult,
      h.in=ids.h[-c(1L, h.len)] +
        rep_each((seq_len(col.hn)-1L) * nr * mult, h.len - 2L),
      h.out.b=ids.h[h.len] + (seq_len(col.hn) - 1L) * nr * mult
    )
    # Errors
    err.child <- if(i > 1L) {
      which <- list(2:3, 1:4, c(1L,4L), 3:4, 1:4, 1:2)
      Map(.get_child_err, ids, which, 'axis')
    }
    err.par <- c(
      Map(.get_par_err, ids[1:3], list(c(-mhalf, mhalf))),
      Map(.get_par_err, ids[4:6], list(c(-mhalf * nr, mhalf * nr)))
    )
    for(j in seq_along(ids)) {
      errors[ids[[j]]] <- do.call(
        pmax, c(list(na.rm=TRUE, errors[ids[[j]]]), err.par[j], err.child[[j]])
    ) }
    # - Diagonals --------------------------------------------------------------

    ids.raw <- seq(mhalf + nr * mhalf + 1L, nr * (mhalf + 1L), by=mult) +
      matrix((seq_len(tile.c) - 1L) * mult * nr, tile.r, tile.c, byrow=TRUE)
    which.sw <- xor((col(ids.raw) %% 2L), (row(ids.raw) %% 2L))
    ids <- list(nw=ids.raw[!which.sw], sw=ids.raw[which.sw])

    err.child <- Map(.get_child_err, ids, list(1:4), 'diag')
    err.par <- Map(
      .get_par_err, ids,
      list((mhalf * nr + mhalf) * c(1L, -1L), (mhalf * nr - mhalf) * c(1L, -1L))
    )
    for(j in seq_along(ids)) {
      errors[ids[[j]]] <- do.call(
        pmax, c(list(na.rm=TRUE, errors[ids[[j]]]), err.par[j], err.child[[j]])
    ) }
  }
  errors
}
