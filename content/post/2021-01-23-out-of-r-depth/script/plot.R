vcf <- colorRamp(viridis(256), space='Lab')
vc <- function(x) {
  nas <- is.na(x)
  x <- x[!nas]
  res <- rep(NA_character_, length(x))
  res[!nas] <- rgb(vcf(x), maxColorValue=255)
  res
}
plot_rast <- function(x, bounds=range(x), invert=FALSE, rast=TRUE) {
  vetr(
    matrix(numeric(), 0, 0), bounds=numeric(2), invert=LGL.1, rast=LGL.1
    # numeric(2) && .[1] < .[2]
  )
  force(bounds)
  oob <- !(x <= bounds[2] & x >= bounds[1])
  x[oob] <- NA
  # if(anyNA(x)) warning("NA or out of range values")
  dat <- (x - min(x, na.rm=TRUE)) / diff(bounds)
  dat <- if(invert) 1 - dat else dat
  cols <- structure(array(vc(dat), dim(x)), class='raster')
  if(rast) plot(cols, interpolate=FALSE)
  else points(row(cols), col(cols), pch=21, bg=cols, cex=1.5)
}


downsample <- function(map, factor=10) {
  ri <- seq(1, nrow(map), by=factor)
  ci <- seq(1, ncol(map), by=factor)
  map2 <- matrix(map[as.matrix(expand.grid(ri, ci))], length(ri))
}
