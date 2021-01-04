# - PNG stuff ------------------------------------------------------------------

# Read in pngs, and write them back stitched together side by side
#
# @param input character(n) vector of pngs to stitch together
# @param output character(1) name of file to write stitched PNG
# @return the output of `png::writePNG`

cbind_pngs <- function(input, output) {
  vetr(character(), character(1L))
  pngs <- lapply(input, png::readPNG)
  png.dims <- vapply(pngs, dim, numeric(3))
  if(length(unique(png.dims[1,])) != 1) stop("different row counts on pngs")
  if(length(unique(png.dims[3,])) != 1) stop("different chanel counts on pngs")

  cols <- sum(png.dims[2,])
  colc <- cumsum(png.dims[2,])
  colcs <- 1L + c(0L, head(colc, 2L))
  d <- array(numeric(), c(png.dims[1,1],sum(png.dims[2,]),png.dims[3,1]))

  for(i in seq_along(colc)) d[,colcs[i]:colc[i],] <- pngs[[i]]
  png::writePNG(d, output)
}
# trim white or near white rows/col (for raster image rows are the x
# coordinatese of the arrays).  We don't bother with alpha as that will require
# us cheking if it has the channel and different handling for it.
#
# @param pngs a list of png arrays (3D), with or without alpha
# @param tol numeric(2), tolerance for rows and cols in that order
# @param pad numeric(2), pixels of padding to add to result will just be full
#   opacity white
# @return list of png arrays

trim_png <- function(pngs, tol=c(2,2), pad=c(0,0)) {
  res <- trim_png_row(pngs, tol[1], pad[1])
  trim_png_col(res, tol[2], pad[2])
}

trim_png_internal <- function(pngs, tol=2, pad=0, which) {
  vetr(
    list() && length(.) > 0, INT.1 && all_bw(., 0, 255), INT.1.POS,
    which=isTRUE(. %in% c('row', 'col'))
  )
  if(which == 'row') {
    sumf <- rowSums
    readf <- function(x, idx) x[idx,,]
    writef <- function(x, idx, val) {
      x[idx,,] <- val
      x
    }
    tar.dim <- 1
    dim.len <- dim(pngs[[1]])[1]
  } else {
    sumf <- colSums
    readf <- function(x, idx) x[,idx,]
    writef <- function(x, idx, val) {
      x[,idx,] <- val
      x
    }
    tar.dim <- 2
    dim.len <- dim(pngs[[1]])[2]
  }
  tol <- tol / 255
  dims <- sapply(pngs, dim)
  if(!is.matrix(dims)) stop("unequal dims")
  if(length(unique(dims[1,])) > 1 || length(unique(dims[2,])) > 1)
    stop("unequal dim2")

  pngsum <- rowSums(sapply(pngs, function(x) sumf(x < 1 - tol))) == 0
  a <- min(which(!pngsum))
  b <- min(which(!rev(pngsum)))

  a.not <- seq_len(a - 1L)
  b.not <- seq_len(b - 1L) + dim.len - b + 1

  tmp <- lapply(pngs, readf, -c(a.not, b.not))
  dim.tmp <- dim(tmp[[1]])
  if(pad) {
    dim.pad <- dim.tmp
    dim.pad[tar.dim] <- dim.pad[tar.dim] + 2*pad
    res.tpl <- array(1, dim.pad)
    lapply(tmp, writef, idx=seq_len(dim.tmp[tar.dim]) + pad, x=res.tpl)
  } else {
    tmp
  }
}
trim_png_row <- function(pngs, tol=2, pad=0) {
  trim_png_internal(pngs, tol, pad, which='row')
}
trim_png_col <- function(pngs, tol=2, pad=0) {
  trim_png_internal(pngs, tol, pad, which='col')
}
