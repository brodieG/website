
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
      x1 <- splits.x[c(x.raw[-nrow(x.raw), -ncol(x.raw)])]
      y1 <- splits.y[c(y.raw[-ncol(x.raw), -ncol(y.raw)])]
      x2 <- splits.x[c(x.raw[-1, -1])]
      y2 <- splits.y[c(y.raw[-1, -1])]
      z1 <- map[cbind(x1, y1)]
      z2 <- map[cbind(x2, y2)]
      zu <- (z1 + z2) / 2
      zE <- zu - map[cbind(round((x1 + x2) / 2), round((y1 + y2) / 2))]
      id <- matrix(seq_along(x1), length(splits.x) - 1L, length(splits.y) - 1L)
      fail <- abs(zE) > thresh
      draw <- fail
} ) }
compute_layers <- function(map, thresh=diff(range(map)) / 50) {
  dim.x <- x <- nrow(map)
  dim.y <- y <- ncol(map)

  layers <- floor(min(log2(c(x, y))))
  diags <- vector("list", layers)

  for(i in seq_len(layers)) {
    mult <- as.integer(2^(i - 1))
    dim.x.prev <- dim.x
    dim.y.prev <- dim.y
    dim.x <- ((x - 1L) %/% mult) * mult + 1L
    dim.y <- ((y - 1L) %/% mult) * mult + 1L

    if(!dim.x || !dim.y) stop("bad dims")

    diags[[i]] <- make_diags(
      seq(1L, to=dim.x, by=mult),
      seq(1L, to=dim.y, by=mult),
      map=map, thresh=thresh
    )
    if(i == 1L) next  # no children when i == 1

    # Map to previously computed child tiles.  Recall that there is one fewer
    # tile than there are "points" in each dimension

    dim.x.child <- (dim.x - 1L)/(mult / 2L)
    dim.y.child <- (dim.y - 1L)/(mult / 2L)
    dim.child <- c(dim.x.child, dim.y.child)

    # @antoine_fabri

    par.ids <- diags[[i]][['id']]
    par.child.ids <- par.ids[
      rep(seq_len(nrow(par.ids)), each=2L),
      rep(seq_len(ncol(par.ids)), each=2L)
    ]
    child.ids <- diags[[i - 1]][['id']][
      seq(1L, to=(dim.x - 1L)/(mult / 2L), by=1L),
      seq(1L, to=(dim.y - 1L)/(mult / 2L), by=1L)
    ]
    # Propagate error by marking every child but the one with the error as a
    # drawable child.  Then parent level tiles that themselves exceed threhsold
    # are marked as drawable.  If we marked every child as drawable including the
    # one that spawned the error we would overdraw.

    child.fail <- diags[[i - 1L]][['fail']][child.ids]
    par.bad <- as.integer(
      names(which(rowsum(child.fail+0L, par.child.ids) > 1L))
    )
    par.bad.children <- child.ids[par.child.ids %in% par.bad]

    diags[[i - 1L]][['draw']][par.bad.children] <-
      !diags[[i - 1L]][['fail']][par.bad.children]
    diags[[i]][par.bad][['fail']] <- TRUE
  }
  diags
}
xx <- compute_layers(elmat1, thresh=5)

dev.close()
dev.new()
old.par <- par(mfrow=c(4,3), mar=numeric(4))
# lapply(xx, function(x) {
#   res <- x[['draw']]
#   dim(res) <- dim(x[['id']])
#   plot(as.raster(res))
# })
lapply(xx, function(x) {
  plot_new(0, 1)
  with(x,
    rect(
      (x1 - 1) / (nrow(elmat1) - 1),
      (y2 - 1) / (ncol(elmat1) - 1),
      (x2 - 1) / (nrow(elmat1) - 1),
      (y1 - 1) / (ncol(elmat1) - 1),
      col=ifelse(draw, 'black', 'white'),
      border=ifelse(draw, 'black', 'white')
    )
  )
})

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
