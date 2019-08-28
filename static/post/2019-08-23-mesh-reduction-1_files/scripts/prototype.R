
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
    seq.x <- seq(1L, length.out=dim.x + 1L, by=mult)
    seq.y <- seq(1L, length.out=dim.y + 1L, by=mult)

    diags[[i]] <- make_diags(seq.x, seq.y, map=map, thresh=thresh)

    # Only failures that don't overlap with child failures should be drawn,
    # So, did any children fail?  We only ned to check on child per parent as by
    # design will set all siblings to fail.

    # Get ids of every child corresponding to a failed tile.

    child.ids <- diags[[i - 1]][['id']]
    child.fail <- if(i > 1L) {
      child.sample <- child.ids[seq_len(dim.x) * 2L, seq_len(dim.y) * 2L]
      diags[[i - 1]][['fail']][child.id.sample]
    } else {
      logical(length(diags[[i]][['id']]))  # no children at first failure
    }
    # If a cell fails a test, then we must draw the children

    diags[[i]][['fail']]

    # is designated to be drawn, then it siblings should be drawn too;
    # we need to figure out how to do this with an error propagation metric
    # instead.

    diags[[i]][['fail']] <- with(diags[[i]], {
      row.base <- (row[fail] - 1L) * 2L
      col.base <- (col[fail] - 1L) * 2L
      child.coord <-
        cbind(c(row.base + 1L, row.base + 2L), c(col.base + 1L, col.base + 2L))
      diags[[i - 1L]][['draw']][child.coord] <-
        diags[[i - 1L]][['draw']][child.coord]

      row.odd <- row %% 2 & TRUE
      col.odd <- col %% 2 & TRUE
      fail[id[(fail & row.odd & row != nrow(row))] + 1L] <- TRUE
      fail[id[(fail & !row.odd & row != 1L)] - 1L] <- TRUE
      fail[id[(fail & col.odd & col != ncol(col))] + nrow(id)] <- TRUE
      fail[id[(fail & !col.odd & col != 1L)] - nrow(id)] <- TRUE
      fail
    })
    diags[[i]] <- within(diags[[i]], draw <- fail & !child.fail)
  }
  diags
}
map <- elmat1
map <- volcano
xx <- compute_layers(map, thresh=5)

# old.par <- par(mfrow=c(3,2), mar=numeric(4))
# lapply(xx, function(x) {
#   res <- x[['draw']]
#   dim(res) <- dim(x[['id']])
#   plot(as.raster(res))
# })
dev.off()
dev.new()
rows <- floor(sqrt(length(xx)))
cols <- ceiling(length(xx) / rows)
old.par <- par(mfrow=c(rows, cols), mar=numeric(4))
#par(mai=numeric(4))
invisible(
  lapply(rev(xx), function(x) {
  plot_new(0, 1)
  rect(0, 0, 1, 1, col='green', border='green')
    x.new <- lapply(x, '[', x[['draw']])
    with(x.new,
      rect(
        (x1 - 1) / (nrow(map) - 1),
        (y2 - 1) / (ncol(map) - 1),
        (x2 - 1) / (nrow(map) - 1),
        (y1 - 1) / (ncol(map) - 1),
        # col = gray((zu - min(map))/diff(range(map))),
        border='black'
        # border = gray((zu - min(map))/diff(range(map)))
        # col=ifelse(draw, gray((zu - min(map))/diff(range(map))), 'green'),
        # border=ifelse(draw, gray((zu - min(map))/diff(range(map))), 'green')
      )
    )
}) )

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
