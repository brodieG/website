

# Alternatively we could get the actual coordinates from the grobs
# * pp <- ggplot_gtable(ggplot_build(p))
# * Look for grobs with right dimension
# * Seems a bit of a pita b/c we have to figure out nesting, etc.
# For our simple case, much easier to just compute directly on the image.

# First find out what cells have gray colors

set.seed(1)
gn <- 10
g2 <- sample(seq_len(gn), 95, replace=TRUE)
x <- runif(length(g2))
o <- order(g2)
go <- g2[o]
xo <- x[o]

png <- png::readPNG('~/Downloads/colsums2/img-012.png') * 255
gray <- which(
  rowSums(
    abs(png[,,1:3] - c(rowSums(png[,,1:3], dims=2)) / 3), dims=2
  ) < 0.00001,
  arr.ind=TRUE
)
# And which color is the dominant gray; this is the panel background

gray.vals <- table(png[cbind(gray, 1L)])
panel.col <- as.numeric(names(gray.vals[which.max(gray.vals)]))

# Find the boundaries of the panel

pnl.pix <- rowSums(abs(png[,,1:3] - panel.col), dims=2) == 0
pnl.pix.w <- which(pnl.pix, arr.ind=TRUE)
stopifnot(nrow(pnl.pix.w) == gray.vals[which.max(gray.vals)])

# We know roughly that 1/8th of the pixels in we're dealing with the map
# portion KEEP IN MIND Y/X flipped to image, below we use the matrix X/Y

tol <- c(1, -1) # pixels to trim down on the ranges

pnl.rle.y <- rle(pnl.pix[nrow(pnl.pix)/4, ])
map.pix.y <- cumsum(pnl.rle.y[['lengths']][1:3])[c(2,3)] + tol
map.pix.y # yup, seems reasonabl

pnl.rle.x <- rle(pnl.pix[, map.pix.y[1] + 2])
map.pix.x <- cumsum(pnl.rle.x[['lengths']][1:3])[c(2,3)] + tol
map.pix.x # yup, seems reasonabl

# figure out the group sum location based on the first locations

pnl.rle.y2 <- rle(pnl.pix[map.pix.x[1] + 2, ])
map.pix.y2 <- cumsum(pnl.rle.y2[['lengths']][1:5])[c(4,5)] + tol
map.pix.y2

pnl.rle.x2 <- rle(pnl.pix[, mean(map.pix.y2)])
map.pix.x2 <- cumsum(pnl.rle.x2[['lengths']][1:3])[c(2,3)] + tol
map.pix.x2 # yup, seems reasonabl

# Generate the coordinates for the tiles, the last coordinate is
# purposefully out of bounds

make_elev <- function(pix.x, pix.y, nrow, len, elev, val) {
  cols <- ceiling(len/nrow)
  ys <- round(seq(from=1, to=diff(pix.y) + 2L, length.out=cols + 1L)) +
    pix.y[1] - 1L
  xs <- round(seq(from=1, to=diff(pix.x) + 2L, length.out=nrow + 1L)) +
    pix.x[1] - 1L

  ymins <- head(ys, -1)
  ymaxs <- tail(ys, -1) - 1
  xmins <- head(xs, -1)
  xmaxs <- tail(xs, -1) - 1

  # Now make the tiles (first 95)

  tiles <- data.frame(
    ymin=rep(ymins, each=nrow), ymax=rep(ymaxs, each=nrow),
    xmin=rep(xmins, nrow), xmax=rep(xmaxs, nrow)
  )[seq_len(len),]

  for(i in seq_len(nrow(tiles))) {
    xins <- seq(from=tiles[i,'xmin'], to=tiles[i,'xmax'], by=1)
    yins <- seq(from=tiles[i,'ymin'], to=tiles[i,'ymax'], by=1)
    elev[xins, yins] <- val[i]
  }
  elev
}
elev.start <- elev.end <- array(0, dim=dim(png)[1:2])

# Start elevation

elev.start <- make_elev(map.pix.x, map.pix.y, 10, 95, elev.start, x)
elev.end <- make_elev(map.pix.x, map.pix.y, 10, 95, elev.end, xo)
elev.end <- make_elev(
  map.pix.x2, map.pix.y2, 1, 10, elev.end, c(rowsum(xo, go))
)

# # downsample
# 
# esmall <- elev.start[1:400,1:400]
# png.start <-  png::readPNG('~/Downloads/colsums2/img-002.png')
# psmall <- png.start[1:400, 1:400,]
# psmallbw <- rowMeans(psmall[,,1:3], dims=2)
# 
# downsample <- function(mx) {
#   mx.tmp <- (mx[, rep(c(T,F), ncol(mx)/2)] + mx[, rep(c(F,T), ncol(mx)/2)]) / 2
#   (
#     mx.tmp[rep(c(T,F), nrow(mx.tmp)/2),] + 
#     mx.tmp[rep(c(F,T), nrow(mx.tmp)/2),]
#   ) / 2
# }

library(shadow)
png.start <-  png::readPNG('~/Downloads/colsums2/img-002.png')
png.end <-  png::readPNG('~/Downloads/colsums2/img-012.png')
png.start.bw <- rowMeans(png.start[,,1:3], dims=2)
png.end.bw <- rowMeans(png.end[,,1:3], dims=2)
render_frame <- function(
  png, elev, r=diag(3), mult=20, d=Inf,
  anglebreaks=45, max.el=max(elev), empty=1
) {
  if(length(dim(png)) == 2) dim(png) <- c(dim(png), 1L)
  shade <- rayshader::ray_shade(
    elev * mult, sunangle=-60, lambert=FALSE,
    anglebreaks=anglebreaks, maxsearch=300
  ) * .6 + .4
  # shift shade up one pixel, this is a horrible hack to try to get the
  # tall bars to be shaded on their side.  Should really be one pixel towards
  # sun direction, but we're being lazy.

  shade <- rbind(shade[-1,], shade[1,])
  shade <- shade[,rev(seq_len(ncol(shade)))]
  res <- vapply(seq_len(dim(png)[3]),
    function(x) {
      # lift a corner so that on rotation resizing consistent
      elev[1L, dim(elev)[2]] <- max.el         # top right hand corner lifted
      png[1:2,dim(elev)[2]-(0:1),x] <- empty   # change this to see corner
      message("Rendering layer ", x)
      shadow::render_elevation_rel(
        elev * mult, png[,,x] * shade, r, zord='pixel', d=d, empty=empty
      )
    },
    png.start[,,1]
  )
  if(dim(res)[3] == 1L) dim(res) <- dim(res)[1:2]
  res
}
render_frames <- function(
  png, elev, rs, ds, angles, breaks, breaks.mult, max.el=max(elev), empty=1,
  mult=20
) {
  stopifnot(
    length(rs) == length(ds),
    length(rs) == length(angles),
    length(rs) == length(breaks.mult)
  )
  lapply(
    seq_along(rs),
    function(x)
      render_frame(
        png, elev, r=rs[[x]], d=ds[[x]],
        anglebreaks=breaks * breaks.mult[[x]] + angles[[x]],
        max.el=max.el, empty=empty, mult=mult
      )
  )
}
el.mult <- 20
el.max.val <- el.mult * max(elev.end)
frame.n <- 50

seq.base <- 5
seq.vals.3 <- seq(from=-(seq.base^3), to=seq.base^3, length.out=frame.n)
sign <- sign(seq.vals.3)
seq.vals <- round((sign * abs(seq.vals.3) ^ (1/3) + seq.base)/ (2*seq.base), 7)

make_path <- function(min, max) {
  rng <- c(min, max)
  diff(rng) * seq.vals + min(rng)
}
# we want our base line distance from base of model to top of model to distance
# to be more or less linear (not entirely correct as it doesn't account for
# rotation)

d.inv.start <- el.max.val / (1e3 - el.max.val)
d.inv.end <- el.max.val / (1e5 - el.max.val)
ds <- rev(el.max.val / make_path(d.inv.end, d.inv.start) + el.max.val)
rs <- lapply(make_path(-20, 0), rot_y)
angles <- make_path(45, 90)
breaks <- seq((-2):2)
breaks.mult <- make_path(1, 0)
# png.root <- '~/Downloads/colsums2/z3d-img-%03d.png'
png.root <- '~/Downloads/colsums-tests/z3d-img-%03d.png'

frames.3d <- render_frames(
  png.end[,,1:3], elev=elev.end, rs=rev(rs), ds=rev(ds), angles=rev(angles),
  breaks=breaks, breaks.mult=rev(breaks.mult), empty=0, mult=el.mult
)
for(i in seq_along(frames.3d)) {
  frame <- frames.3d[[i]]
  png::writePNG(frame, sprintf(png.root, i))
}
frames.3d2 <- render_frames(
  png.start[,,1:3], elev=elev.start, rs=rs, ds=ds, angles=angles,
  breaks=breaks, breaks.mult=breaks.mult, max.el=max(elev.end), empty=0
)
png.root <- '~/Downloads/colsums-tests/3d-start-img-%03d.png'
for(i in seq_along(frames.3d2)) {
  frame <- frames.3d2[[i]]
  png::writePNG(frame, sprintf(png.root, i))
}

zz <- render_frame(
  png.end[,,1:3], elev.end, r=rot_y(-20), anglebreaks=45, d=1e3, empty=0
)
plot(as.raster(zz))
zz <- render_frame(
  png.end[,,1:3], elev.end, r=rot_y(0), anglebreaks=90, d=Inf, empty=0
)
plot(as.raster(round(zz*255)/255))


#ffmpeg -pattern_type glob -i '*.png' -r 30 -pix_fmt yuv420p out.mp4
#ffmpeg -pattern_type glob -i '*.png' -vf "fps=5,format=yuv420p" out.mp4
#ffmpeg -r 1/5 -i img%03d.png -c:v libx264 -vf "fps=25,format=yuv420p" out.mp4
"
