
compute_error3 <- function(map) {
  if(!all(dim(map) %% 2L) || min(dim(map)) <= 2L) stop("invalid map")
  .pmax2 <- function(a, b) do.call(pmax, c(list(a, na.rm=TRUE), b))
  # offsets are row/col, start at parent and go clockwise, offsets are
  # already multiplied by 2L b/c otherwise we would have fractional offsets
  # for smallest square set.
  offset.dg <- aperm(
    array(
      c(
        1L,1L, 0L,0L, 2L,2L, 1L,2L, 0L,1L,
        1L,1L, 0L,0L, 2L,2L, 1L,0L, 2L,0L,
        3L,1L, 4L,0L, 2L,2L, 3L,2L, 4L,1L,
        3L,1L, 4L,0L, 2L,2L, 2L,1L, 3L,0L,
        1L,3L, 2L,0L, 0L,4L, 1L,4L, 2L,3L,
        1L,3L, 2L,0L, 0L,4L, 0L,3L, 1L,2L,
        3L,3L, 2L,2L, 4L,4L, 3L,4L, 2L,3L,
        3L,3L, 2L,2L, 4L,4L, 3L,2L, 4L,2L
      ),
      dim=c(2L, 5L, 8L)
    ),
    c(3L, 1L, 2L)
  )
  offset.ax <- aperm(
    array(
      c(
        2L,0L, 0L,0L, 4L,0L, 4L,1L, 1L,1L,
        4L,2L, 4L,0L, 4L,4L, 3L,3L, 3L,1L,
        2L,4L, 4L,4L, 0L,4L, 1L,3L, 4L,3L,
        0L,2L, 0L,4L, 0L,0L, 1L,1L, 3L,1L
      ),
      dim=c(2L, 5L, 4L)
    ),
    c(3L, 1L, 2L)
  )
  nr <- nrow(map)
  nc <- ncol(map)
  layers <- floor(min(log2(c(nr, nc) - 1L)))
  errors <- array(0, dim=dim(map))

  # # Force tile splits in areas that don't fall in 2^layers squares

  # if((r.extra <- (nr - 1L) %% 2L^layers)) {
  #   while(r.extra - 2^(floor(log2(r.extra))))
  #     r.extra <- r.extra - 2^(floor(log2(r.extra)))
  #   errors[nr, seq(r.extra / 2L + 1L, nc, by=r.extra)] <- Inf
  # }
  # if((c.extra <- (nc - 1L) %% 2L^layers)) {
  #   while(c.extra - 2^(floor(log2(c.extra))))
  #     c.extra <- c.extra - 2^(floor(log2(c.extra)))
  #   errors[seq(c.extra / 2L + 1L, nr, by=c.extra), nc] <- Inf
  # }
  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    tile.nr <- ((nr - 1L) %/% mult)
    tile.nc <- ((nc - 1L) %/% mult)

    for(j in c('axis','diag')) {
      o <- if(j == 'diag') offset.dg else offset.ax

      if(i < 2L && j == 'axis') {
        o[,,1:3] <- o[,,1:3] %/% 2L
      } else {
        o <- o * (mult %/% 2L)
      }
      if(j == 'diag') {
        if(tile.nr < 2L) o <- o[c(1L,2L,5L,6L),,]
        if(tile.nc < 2L) o <- o[seq_len(dim(o)[1L]/2L),,]
      }
      o.1 <- c(o[,1L,] + o[,2L,] * nr + 1L)
      o.2 <- o.1 + rep((seq_len(tile.nr) - 1L) * mult, each=length(o.1))
      o.3 <- o.2 + rep((seq_len(tile.nc) - 1L) * mult, each=length(o.2))
      olen <- length(o.3)/5L
      o.id <- seq_len(olen)

      err.list <- vector('list', if(i < 2L) 2L else 4L)
      err.list[[1L]] <- abs(
        map[o.3[o.id]] - (map[o.3[o.id + olen]] + map[o.3[o.id + olen * 2L]])/2
      )
      err.list[[2L]] <- errors[o.3[o.id]]
      if(i >= 2L || j == 'diag') {
        err.list[[3L]] <- errors[o.3[o.id + olen * 3L]]
        err.list[[4L]] <- errors[o.3[o.id + olen * 4L]]
      }
      errors[o.3[o.id]] <- do.call(pmax, err.list)
  } }
  errors
}
