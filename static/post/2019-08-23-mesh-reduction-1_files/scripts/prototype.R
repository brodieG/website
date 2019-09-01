
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
compute_error <- function(map) {
  dim.x <- x <- nrow(map)
  dim.y <- y <- ncol(map)

  layers <- floor(min(log2(c(x, y))))
  diags <- vector("list", layers)

  errors <- array(0, dim=dim(map))

  for(i in seq_len(layers)) {
    mult <- as.integer(2^i)
    dim.x <- ((x - 1L) %/% mult)
    dim.y <- ((y - 1L) %/% mult)
    seq.x <- seq(1L, length.out=dim.x + 1L, by=mult)
    seq.y <- seq(1L, length.out=dim.y + 1L, by=mult)

    x.id <- .row(c(length(seq.x), length(seq.y)))
    y.id <- .col(c(length(seq.x), length(seq.y)))
    row <- x.id[-nrow(x.id), -ncol(x.id)]
    col <- y.id[-ncol(y.id), -ncol(y.id)]
    x1 <- seq.x[c(row)]
    y1 <- seq.y[c(col)]
    xu <- x1 + (mult / 2L)
    x2 <- seq.x[c(x.id[-1, -1])]
    y2 <- seq.y[c(y.id[-1, -1])]
    yu <- y1 + (mult / 2L)

    top.id <- cbind(xu, y1)
    top.zE <- (map[cbind(x1, y1)] + map[cbind(x2, y1)]) / 2L - map[top.id]
    errors[top.id] <- top.zE

    left.id <- cbind(x1, yu)
    left.zE <- (map[cbind(x1, y1)] + map[cbind(x1, y2)]) / 2L - map[left.id]
    errors[left.id] <- left.zE

    diag.id <- cbind(xu, yu)
    diag.zE <- (map[cbind(x1, y1)] + map[cbind(x2, y2)]) / 2L - map[diag.id]
    errors[diag.id] <- diag.zE
  }
  errors
}
errors <- compute_error(map)
err.ind <- which(errors > 20, arr.ind=TRUE)

# For each error, we need to draw all the corresponding triangles.  This means
# we need to figure out the size of the "diagonal", and whether we're dealing
# with a diamond or square.


# we need to compute even level odd level tiles.  For odd tiles (1 index), we
# check the actual diagonal, i.e. tile is square.  For even tiles, we treat the
# tile as a rhombus so the diagonal is actually parallel to the x axis (or y
# axis depending on how you draw it)?



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
