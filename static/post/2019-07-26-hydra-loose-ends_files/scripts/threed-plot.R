

# Alternatively we could get the actual coordinates from the grobs
# * pp <- ggplot_gtable(ggplot_build(p))
# * Look for grobs with right dimension
# * Seems a bit of a pita b/c we have to figure out nesting, etc.
# For our simple case, much easier to just compute directly on the image.

png <- png::readPNG('~/Downloads/colsums2/img-012.png') * 255
RNGversion("3.5.2"); set.seed(42)
set.seed(1)
gn <- 10
g2 <- sample(seq_len(gn), 95, replace=TRUE)
o <- order(g2)
go <- g2[o]
x <- runif(length(g2))
xo <- x[o]

# First find out what cells have gray colors

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


# With the tiles we can create an elevation map using the original values, there
# are only 95 tiles so we're going to be lazy and do them in a for loop

#
# Use the elevation map to compute shade

# shade <- shadow::ray_shade2(elev * , sunangle=315-90, anglebreaks=seq(30,60,1))

shade_png <- function(png, root, elev, angles, deltas, delta.fac) {
  png.orig <- png
  for(i in seq_along(angles)) {
    png <- png.orig
    shade <- rayshader::ray_shade(
      elev * 25, sunangle=-60, lambert=FALSE,
      anglebreaks=angles[i] + (deltas * delta.fac[i]), maxsearch=300
    ) * .7 + .3
    png[,,1:3] <- png[,,1:3] * c(shade[,rev(seq_len(ncol(shade)))])
    png::writePNG(png, sprintf(root, i))
  }
}
angles <- seq(45, 90, by=5)
deltas <- ((-5):5) / 10
delta.fac <- seq(1, 0, length.out=length(deltas))

shade_png(
  png::readPNG('~/Downloads/colsums2/img-012.png'),
   '~/Downloads/colsums2/rs-img-%03d.png',
  elev.end, rev(angles), rev(deltas), rev(delta.fac)
)
shade_png(
  png::readPNG('~/Downloads/colsums2/img-002.png'),
   '~/Downloads/colsums2/as-img-%03d.png',
  elev.start, angles, deltas, delta.fac
)

# remove color profile form existing files

start <- 2;
end <- 12;

png.root <- '~/Downloads/colsums2/img-%03d.png'
for(i in seq(start, end, by=1)) {
  png.tmp <- png::readPNG(sprintf(png.root, i))
  png::writePNG(png.tmp, sprintf(png.root, i))
}

png.test <- png::readPNG()
png::writePNG(png.test, '~/Downloads/colsums2/img-012a.png')
png.test2 <- png::readPNG('~/Downloads/colsums2/img-012a.png')

par(mai=numeric(4))

# downsample

esmall <- elev.start[1:400,1:400]
png.start <-  png::readPNG('~/Downloads/colsums2/img-002.png')
psmall <- png.start[1:400, 1:400,]
psmallbw <- rowMeans(psmall[,,1:3], dims=2)

downsample <- function(mx) {
  mx.tmp <- (mx[, rep(c(T,F), ncol(mx)/2)] + mx[, rep(c(F,T), ncol(mx)/2)]) / 2
  (
    mx.tmp[rep(c(T,F), nrow(mx.tmp)/2),] + 
    mx.tmp[rep(c(F,T), nrow(mx.tmp)/2),]
  ) / 2
}

library(shadow)
png.start <-  png::readPNG('~/Downloads/colsums2/img-002.png')
png.end <-  png::readPNG('~/Downloads/colsums2/img-012.png')
png.start.bw <- rowMeans(png.start[,,1:3], dims=2)
png.end.bw <- rowMeans(png.end[,,1:3], dims=2)
render_frame <- function(
  png, elev, r=diag(3), mult=20, d=Inf, anglebreaks=45, max.el=max(elev)
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
      elev[1L, dim(elev)[2]] <- max.el  # top right hand corner lifted
      png[1L,dim(elev)[2],x] <- 0
      message("Rendering layer ", x)
      shadow::render_elevation_rel(
        elev * mult, png[,,x] * shade, r, zord='pixel', d=d, empty=1
      )
    },
    png.start[,,1]
  )
  if(dim(res)[3] == 1L) dim(res) <- dim(res)[1:2]
  res
}
render_frames <- function(
  png, elev, rs, ds, angles, breaks, breaks.mult, max.el=max(elev)
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
        max.el=max.el
      )
  )
}
frame.n <- 10
d.start <- 1e4
d.end <- 1e3
ds <- c(Inf, seq(sqrt(d.start), 0, length.out=frame.n - 1)^2 + d.end)
frames.3d <- render_frames(
  png.end[,,1:3], elev=elev.end,
  rs=lapply(seq(0,-20,length.out=frame.n), rot_y),
  ds=ds,
  angles=seq(90, 45, length.out=frame.n),
  breaks=seq((-2):2), breaks.mult=seq(0, 1, length.out=frame.n)
)
png.root <- '~/Downloads/colsums2/3d-img-%03d.png'
for(i in seq_along(frames.3d)) {
  frame <- frames.3d[[i]]
  png::writePNG(frame, sprintf(png.root, i))
}
frames.3d2 <- render_frames(
  png.start[,,1:3], elev=elev.start,
  rs=lapply(seq(0,-20,length.out=frame.n), rot_y),
  ds=ds,
  angles=seq(90, 45, length.out=frame.n),
  breaks=seq((-2):2), breaks.mult=seq(0, 1, length.out=frame.n),
  max.el=max(elev.end)
)

png3d <- render_frames(
  png.end[,,1:3], elev.end, r=rot_y(-10), d=1000
)
par(mai=numeric(4))
plot(as.raster(round(png3d*255)/255))

el <- matrix(0, 100, 100)
tx <- matrix(.8, 100, 100)

xx <- shadow::render_elevation_rel(
  el, tx, rot_x(0), zord='pixel', d=10, empty=0
)


png('~/Downloads/tmp.png', width=dim(pngbw)[2], height=dim(pngbw)[1])
par(mai=numeric(4))
plot(as.raster(xx))
dev.off()

flip <- function(x) t(x)[rev(seq_len(ncol(x))),]
par(mai=numeric(4))
plot(as.raster(xx))

elevation <- exs * 50
texture <- ps
rotation <- rot_x(45) %*% rot_y(-45)
rotation <- rot_y(-10)
zord <- 'mesh'

rl <- shadow:::rotate(
  elevation = elevation, texture = texture, rotation = rotation
)
rlp <- persp_rel(rl, 1)
resolution <- 400L
rlps <- shadow:::scale_rel(rlp, resolution)
mesh <- shadow:::mesh_tri(rlps, dim(elevation), order = FALSE)

# ## Rescale data to a range from 0 to `range` where `range` in (0,1]
# rescale <- function(x, range=1, center=0.5)
#   ((x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))) * range +
#    (1 - range) * center
# 
# ## Prepare a plot with a particular aspect ratio
# plot_new <- function(
#   x, y, xlim=c(0,1), ylim=c(0,1),
#   par.args=list(mai=numeric(4L), xaxt='n', yaxt='n', xaxs='i', yaxs='i')
# ) {
#   if(length(par.args)) do.call(par, par.args)
#   plot.new()
#   plot.window(
#     xlim, ylim, asp=diff(range(y, na.rm=TRUE))/diff(range(x, na.rm=TRUE))
# ) }
# 
# mesh.tri <- mesh
# x <- do.call(rbind, c(mesh.tri[,'x'], list(NA)))
# y <- do.call(rbind, c(mesh.tri[,'y'], list(NA)))
# col <- rep(c('#CCCCFF','#CCFFCC'), each=ncol(x)/2)
# plot_new(x, y)
# polygon(rescale(x), rescale(y), col=col)
# plot_new(rlp$x,rlp$y)
# points(rescale(rlp$x), rescale(rlp$y), col=gray(rlp$t), pch=16)

res <- shadow:::rasterize(mesh, attr(rlps, "resolution"), 'pixel', 0)
plot(as.raster(res))

par(mai=numeric(4))
plot(as.raster(res))


mesh.tri <- mesh
lim <- 120000
off <- 0
idx <- seq(lim * 4) + off * 4
x <- do.call(rbind, c(mesh.tri[,'x'], list(NA)))
y <- do.call(rbind, c(mesh.tri[,'y'], list(NA)))
texture <- gray((Reduce('+', mesh.tri[,'t'])/nrow(mesh.tri)))
plot_new(x, y)
polygon(rescale(x), rescale(y), col=texture, border=texture)




ggplot(tiles) + geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
