

# Alternatively we could get the actual coordinates from the grobs
# * pp <- ggplot_gtable(ggplot_build(p))
# * Look for grobs with right dimension
# * Seems a bit of a pita b/c we have to figure out nesting, etc.
# For our simple case, much easier to just compute directly on the image.

png <- png::readPNG('~/Downloads/colsums2/img-001.png') * 255
RNGversion("3.5.2"); set.seed(42)
set.seed(1)
gn <- 10
g2 <- sample(seq_len(gn), 95, replace=TRUE)
x <- runif(length(g2))
xo <- x[order(g2)]

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
# portion

pnl.rle.100 <- rle(pnl.pix[, ncol(pnl.pix)/8])
map.pix.y <- cumsum(pnl.rle.100[['lengths']][1:3])[c(2,3)]
map.pix.y # yup, seems reasonabl

pnl.rle.100 <- rle(pnl.pix[nrow(pnl.pix)/8, ])
map.pix.x <- cumsum(pnl.rle.100[['lengths']][1:3])[c(2,3)]
map.pix.x # yup, seems reasonabl

# Generate the coordinates for the tiles, the last coordinate is
# purposefully out of bounds

ys <-
  round(seq(from=1, to=diff(map.pix.y) + 2L, length.out=11)) + map.pix.y[1] - 1L
xs <- 
  round(seq(from=1, to=diff(map.pix.x) + 2L, length.out=11)) + map.pix.x[1] - 1L

ymins <- head(ys, -1)
ymaxs <- tail(ys, -1) - 1
xmins <- head(xs, -1)
xmaxs <- tail(xs, -1) - 1

# Now make the tiles (first 95)

tiles <- data.frame(
  ymin=rep(ymins, 10), ymax=rep(ymaxs, 10),
  xmin=rep(xmins, each=10), xmax=rep(xmaxs, each=10)
)[1:95,]

# With the tiles we can create an elevation map using the original values, there
# are only 95 tiles so we're going to be lazy and do them in a for loop

elev <- array(0, dim=dim(png)[1:2])

for(i in seq_len(nrow(tiles))) {
  xins <- seq(from=tiles[i,'xmin'], to=tiles[i,'xmax'], by=1)
  yins <- seq(from=tiles[i,'ymin'], to=tiles[i,'ymax'], by=1)
  elev[yins, xins] <- x[i]
}
#
# Use the elevation map to compute shade

shade <- shadow::ray_shade2(elev * 100, sunangle=315-90, anglebreaks=seq(30,60,1))
shade <- rayshader::ray_shade(
  elev * 100, sunangle=-40, anglebreaks=seq(30,60,1)
)
png.fin <- png/255
png.fin[,,1:3] <- png.fin[,,1:3] * c(shade[,rev(seq_len(ncol(shade)))])
plot(as.raster(png.fin))

# png.fin <- array(0, dim(png))
png.fin[,,4] <- elev
png::writePNG(png.fin, "~/Downloads/elev.png")


ggplot(tiles) + geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
