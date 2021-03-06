source('static/script/mesh-viz/viz-lib.R')
dir <- 'static/post/2019-08-23-mesh-reduction-1_files/images/post-proc'
dir.tar <- 'static/post/2019-08-23-mesh-reduction-1_files/images/post-proc-res'
allf <- list.files(dir, full.names=TRUE)


trim_and_copy <- function(f, tar, tol=3, plot=FALSE) {
  png.in <- lapply(f, png::readPNG)
  png.trim <- trim_png_row(png.in, 3)

  if(plot) {
    col <- floor(sqrt(length(f)))
    row <- ceiling(length(f) / col)
    par(bg='blue', mfrow=c(row,col), mai=numeric(4))
    lapply(png.trim, function(x) plot(as.raster(x)))
  } else {
    lapply(
      seq_along(png.trim),
      function(id) {
        png::writePNG(
          png.trim[[id]], file.path(dir.tar, basename(f)[[id]])
    ) } )
  }
}
allf[grep('(composite|water).*fin.*1', allf)]

trim_and_copy(
  allf[grep('(composite|water).*fin.*1', allf)], tar=dir.tar, plot=FALSE
)
trim_and_copy(
  allf[grep('(composite|water).*fin.*2', allf)], tar=dir.tar, plot=FALSE
)
trim_and_copy(
  allf[grep('(composite|water).*fin.*3', allf)], tar=dir.tar, plot=FALSE
)
trim_and_copy(
  allf[grep('(composite|water).*fin.*4', allf)], tar=dir.tar, plot=FALSE
)

hexaptych.f <- allf[grep('(composite|water).*fin', allf)][c(4,1,5,2,6,3)]
hexaptych.in <- lapply(hexaptych.f, png::readPNG)
hexaptych <- trim_png_row(hexaptych.in, 3)


lapply(
  seq_along(hexaptych),
  function(id) {
    png::writePNG(
      hexaptych[[id]], file.path(dir.tar, basename(hexaptych.f)[[id]])
) } )

dip.carry.f <- allf[grep('carry', allf)]
dip.carry.in <- lapply(dip.carry.f, png::readPNG)
dip.carry <- trim_png_row(dip.carry.in, 6)

par(bg='blue', mfrow=c(1,2), mai=numeric(4))
lapply(dip.carry, function(x) plot(as.raster(x)))

lapply(
  seq_along(dip.carry),
  function(id) {
    png::writePNG(
      dip.carry[[id]], file.path(dir.tar, basename(dip.carry.f)[[id]])
) } )

surf.err.f <- allf[grep('^err-(simple|volc)\\.', basename(allf))]
surf.err.in <- lapply(surf.err.f, png::readPNG)
surf.err <- trim_png_row(surf.err.in, 6)
surf.err <- lapply(surf.err, aperm, c(2, 1, 3))
surf.err <- trim_png_row(surf.err, 6)
surf.err <- lapply(surf.err, aperm, c(2, 1, 3))
surf.err <- lapply(
  surf.err,
  function(x) {
    d <- dim(x)
    res <- array(1, d + c(20, 20, 0))
    res[seq_len(d[1]) + 10, seq_len(d[2]) + 10,] <- x
    res
  }
)

par(bg='blue', mfrow=c(1,2), mai=numeric(4))
lapply(surf.err, function(x) plot(as.raster(x)))

lapply(
  seq_along(surf.err),
  function(id) {
    png::writePNG(
      surf.err[[id]], file.path(dir.tar, basename(surf.err.f)[[id]])
) } )

surf.errl.f <- allf[grep('^err-volc-large', basename(allf))]
surf.errl.in <- lapply(surf.errl.f, png::readPNG)
surf.errl <- trim_png_row(surf.errl.in, 6)
surf.errl[[1]] <- aperm(surf.errl[[1]], c(2, 1, 3))
surf.errl <- trim_png_row(surf.errl, 6)
surf.errl[[1]] <- aperm(surf.errl[[1]], c(2, 1, 3))
surf.tmp <- array(1, dim(surf.errl[[1]] )+ c(50, 50, 0))
surf.tmp[
  seq_len(dim(surf.errl[[1]])[1]) + 25,
  seq_len(dim(surf.errl[[1]])[2]) + 25,
] <- surf.errl[[1]]
surf.errl[[1]] <- surf.tmp

par(bg='blue', mfrow=c(1,1), mai=numeric(4))
lapply(surf.errl, function(x) plot(as.raster(x)))

lapply(
  seq_along(surf.errl),
  function(id) {
    png::writePNG(
      surf.errl[[id]], file.path(dir.tar, basename(surf.errl.f)[[id]])
) } )

simple.m.f <- allf[grep('^simple-mesh-fin', basename(allf))]
simple.m.in <- lapply(simple.m.f, png::readPNG)
simple.m <- trim_png_row(simple.m.in, 4)

par(bg='blue', mfrow=c(1,3), mai=numeric(4))
lapply(simple.m, function(x) plot(as.raster(x)))

lapply(
  seq_along(simple.m),
  function(id) {
    png::writePNG(
      simple.m[[id]], file.path(dir.tar, basename(simple.m.f)[[id]])
) } )


