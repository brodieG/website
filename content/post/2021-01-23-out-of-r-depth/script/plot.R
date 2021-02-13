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
  if(length(factor) == 1) factor <- rep(factor, 2)
  ri <- seq(1, nrow(map), by=factor[1])
  ci <- seq(1, ncol(map), by=factor[2])
  map2 <- matrix(map[as.matrix(expand.grid(ri, ci))], length(ri))
}

colors <- list(
  land=c('chartreuse4', 'wheat3', 'whitesmoke'),
  water=rev(c('blue4', 'dodgerblue4', 'dodgerblue'))
)

map_to_col <- function(
  map, water,
  colors=list(
    land=terrain.colors(10),
    water=c('blue4', 'dodgerblue4', 'dodgerblue')
  )
) {
  vetr(
    matrix(numeric(), 0, 0), LGL && length(.) == length(map),
    list(land=character(), water=character())
  )
  m_to_c_int <- function(map, cols) {
    cc <- colorRamp(cols, space='Lab')
    rgb <- cc((map - min(map, na.rm=TRUE))/diff(range(map, na.rm=TRUE)))
    col <- rep(NA_character_, length(map))
    col[!is.na(map)] <- rgb(rgb[!is.na(map),], maxColorValue=255)
    col
  }
  height <- map
  height[water] <- NA
  height.c <- m_to_c_int(height, colors[['land']])

  depth <- map
  depth[!water] <- NA
  depth.c <- m_to_c_int(depth, colors[['water']])

  all.col <- height.c
  all.col[!is.na(depth.c)] <- depth.c[!is.na(depth.c)]
  structure(array(all.col, dim(map)), class='raster')
}

