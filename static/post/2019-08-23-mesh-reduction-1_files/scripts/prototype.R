
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
      zu <- (z1 + z2) / 2
      zE <- zu - map[cbind((x1 + x2) / 2, (y1 + y2) / 2)]
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
    dim.x <- ((x - 1L) %/% mult)
    dim.y <- ((y - 1L) %/% mult)

    if(!dim.x || !dim.y) stop("bad dims")

    diags[[i]] <- make_diags(
      seq(1L, length.out=dim.x + 1L, by=mult),
      seq(1L, length.out=dim.y + 1L, by=mult),
      map=map, thresh=thresh
    )
    if(i == 1L) next  # no children when i == 1

    # Only failures that don't overlap with child failures should be drawn,
    # So, did any children fail?  We only ned to check on child per parent as by
    # design will set all siblings to fail.

    child.id <- diags[[i - 1]][['id']][seq_len(dim.x - 1), seq_len(dim.y - 1)]
    child.id.seq <- seq_along(child.id)
    child.fail <- diags[[i - 1]][['fail']][
      child.id[child.id.seq %% 2 & col(child.id) %% 2]
    ]
    diags[[i]][['draw']][child.fail] <- FALSE

    # If a cell is designated to be drawn, then it siblings should be drawn too;
    # we need to figure out how to do this with an error propagation metric
    # instead.

    diags[[i]][['draw']] <- with(diags[[i]], {
      row.odd <- as.logical(row %% 2)
      col.odd <- as.logical(col %% 2)
      draw[id[(draw & row.odd)] + 1L] <- TRUE
      draw[id[(draw & !row.odd)] - 1L] <- TRUE
      draw[id[(draw & col.odd)] + nrow(id)] <- TRUE
      draw[id[(draw & !col.odd)] - nrow(id)] <- TRUE
      draw
    })
  }
  diags
}
# map <- elmat1
map <- volcano
xx <- compute_layers(map, thresh=2)

dev.off()
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
      (x1 - 1) / (nrow(map) - 1),
      (y2 - 1) / (ncol(map) - 1),
      (x2 - 1) / (nrow(map) - 1),
      (y1 - 1) / (ncol(map) - 1),
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
