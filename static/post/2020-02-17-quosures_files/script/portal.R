source('static/script/mesh-viz/viz-lib.R')
source('static/post/2020-02-17-quosures_files/script/helper.R')
source('static/post/2020-02-17-quosures_files/script/bag.R')
library(svgchop)
library(rayrender)

# - Logo Data ------------------------------------------------------------------

url <- '~/Downloads/rlang/rlang2.svg'
zz <- parse_svg(url)
ww <- interp_paths(zz, normalize=TRUE)
paths <- lapply(
  ww,
  function(x) {
    res <- x[['coords']]
    res[[1]] <- res[[1]] - .5
    res[[2]] <- res[[2]] - .5
    res
  }
)
# separate out the stars (not all polygons are stars)

cl <- vapply(zz, '[[', '', 'class')
types <- attr(zz, 'type')
stars <- paths[types == 'polygon' & cl %in% c('st4734', 'st4733')]
# inner/outer hexagons, unused by us for now
other <- paths[types == 'polygon' & (!cl %in% c('st4734', 'st4733'))]
# Letters, and outside frame

letters <- paths[types == 'path' & cl == 'st4732']
frame <- paths[types == 'path' & cl == 'st4736']

# # Validation
# lett2 <- ww[types == 'path' | (cl %in% c('st4734', 'st4733'))]
# subpaths <- lapply(
#   lett2, function(x) {
#     if(!is.null(attr(x$coords, 'starts')))
#       x$coords[attr(x$coords, 'starts'),] <- NA
#     x$coords$y <- 1 - x$coords$y # need to flip y values
#     x$coords$x <- x$coords$x
#     x$coords
# } )
# par(mai=numeric(4))
# plot.new()
# invisible(
#   Map(
#     polypath, subpaths, col=c('red'),
#     border=NA, rule='evenodd'
# ) )
# - Settings -------------------------------------------------------------------

# There are several hard coded numbers and bad assumptions that may cause
# problems if these settings are changed.  For example:
#
# We're terrible about signs of distances, particularly along the z axes.  Many
# things that are in negative z territory (i.e. past the portal) are treated as
# positive and then negated before final use.
#
# The distancec of the initial observer is assumed to be 1, and the portal at 0.

brick.depth <- 14
depth <- 20

# this is messed up, y and z are swapped due to original extrusion being on
# floor

obs <- c(0, 1, 0)
obsz <- obs[c(1,3,2)]
fov <- 60

gold_mat <- microfacet(
  roughness=0.1, eta=c(0.216,0.42833,1.3184), kappa=c(3.239,2.4599,1.8661)
)
gold <- "#BB5500"
extrude_path <- function(x, ...) {
  extruded_polygon(
    x, holes=if(length(attr(x, 'starts'))) attr(x, 'starts'), ...
  )
}
# - Compute Path ---------------------------------------------------------------

message("path")

# Hex is a mess, we need to clean it up

hex <- frame[[1L]]
outer <- c(3, 6, 7, 10, 13, 14, 3)
hex <- hex[c(outer, 17:nrow(hex)),]
attr(hex, 'starts') <- 8

# brick.start/depth should really be negative

brick.start <- 0.0
y.rise <- .75
angle <- atan(y.rise / (brick.depth - brick.start)) / pi * 180
h <- sqrt(y.rise^2 + (brick.depth - brick.start)^2)

z.obs <- obs[2]

# attempt to project our manual approximation of the path.  We need to find the
# intersection of the paths from our observer through the control points onto
# the path plane.

box <- attr(zz, 'box')
brick.path <- data.frame(
  x=c(260, 530, 530, -100)/box[3] - .5,
  y=1 - c(850, 430, 430,  388)/box[4]
)
brick.dots <- bezier_interp_even(brick.path, 20)
b.p.obj <- dplyr::bind_rows(
  lapply(
    seq_along(brick.dots[[1]]),
    function(i)
      sphere(
        radius=.01, x=brick.dots[[1]][i], y=brick.dots[[2]][i],
        material=diffuse(color='blue')
) ) )
# actual projected version; tweaks to origin path to match under perspective
# https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection

brick.path.2 <- brick.path
brick.path.2[2:3, 1] <- c(.18)
# brick.path.2[2:3, 2] <- c(.47)

# define plane from points p0,1,2
p0 <- c(0, 0, -brick.start)
p1 <- c(0, y.rise, -brick.depth)
p2 <- c(1, y.rise, -brick.depth)
p01 <- p1 - p0
p02 <- p2 - p0

# lines from observer to plane z=0
la <- obsz + c(0,.5,0)
lab <- t(as.matrix(transform(brick.path.2, z=0))) - la

# cross prod: p01 x p02
p01xp02 <- c(xprod(p01, p02))

# Solve for t; careful, get in trouble when we don't intersect
# the plane, i.e. if y.rise is too short for brick.depth
t <- sum(p01xp02 * (la - p0)) /    # (p01 x p02) . (la - p0)
     colSums(-lab * p01xp02)       #  -lab . (p01 x p02)
int <- la + lab * rep(t, each=3)

ang.rad <- angle / 180 * pi
rx <- matrix(
  c(1, 0, 0,
    0,cos(ang.rad),sin(ang.rad),
    0,-sin(ang.rad),cos(ang.rad)
  ), 3
)
int.rot <- t(int) %*% rx

# Interpolate and rotate back

n.rows <- 900
int.dots <- bezier_interp_even(list(x=int.rot[,1], y=int.rot[,3]), n.rows)
int.dots.3d <- rx %*% rbind(int.dots[[1]], 0, int.dots[[2]])

# Place each rung in between the points, compute angle from vector

int.diff <- int.dots.3d[,-1] - int.dots.3d[,-ncol(int.dots.3d)]
int.diff.n <- int.diff / rep(sqrt(colSums(int.diff ^ 2)), each=3)
int.mid <- int.dots.3d[,-ncol(int.dots.3d)] + int.diff/2

# Need to generate a list of all object coordinates, project their centroid
# along the "bag" and onto the outer hex, and if it is outside, remove it.

# Project onto x-y
# Collapse all coodinates into upper right (abs coordinates relative to
# centerpoint, then inside bounding box but outside triangle.
#
# This means we'll need to compute the coordinates directly instead of relying
# on the group object

# a row of pavers

set.seed(1)
off.raw <- rnorm(n.rows - 1) * .03          # noise in row position
row.ang <- -asin(int.diff.n[1,])/pi*180     # angle in x/z
row.ang2 <- -asin(int.diff.n[1,])/pi*180 - 90

off.x <- -off.raw * int.diff.n[3,]
off.z <- off.raw * int.diff.n[1,]

# sum(int.diff.n[,1] * vec) == 0
# a[1] * b[1] + a[2] * b[2] == 0

paver.n <- 15
paver.w <- 1/paver.n
paver.x <- seq(-.5 + paver.w / 2, .5 - paver.w / 2, length.out=paver.n)

pv.x.all <- rbind(
  rep(paver.x, ncol(int.mid)) + rep(off.raw, each=paver.n),
  0,
  0,
  rep(row.ang/180*pi, each=paver.n)
)
pv.rot <- apply(
  pv.x.all, 2,
  function(coords) {
    p <- -coords[4]
    Ry <- matrix(c(cos(p), 0, -sin(p), 0, 1, 0, sin(p), 0, cos(p)), 3)
    t(coords[-4]) %*% Ry
  }
)
pv.all <- int.mid[,rep(seq_len(ncol(int.mid)), each=paver.n)] + pv.rot
pv.all.n <- ncol(pv.all)
pv.colors <- rgb(
  sample(135:185, pv.all.n, replace=TRUE),
  sample(45:75, pv.all.n, replace=TRUE),
  0, maxColorValue=255
)
# determine which pavers are in-bounds, logo plane assumed to be at z==0
# We use obs[2] b/c observer was originallly looking down Y axis, not Z

# pv.all.mult <- obs[2] / (obs[2] - pv.all[3,])
# pv.all.0 <- pv.all
# pv.all.0[1:2,] <-
#   (pv.all.0[1:2,] - c(0, .5)) * rep(pv.all.mult, each=2)
# pv.all.save <- pv.all.0
#
# pv.all.0[1:2,] <- abs(pv.all.0[1:2,])
#
# # oob bounding box
# hex.oob <- hex * .98
#
# pv.oob <-
#   pv.all.0[1,] > max(hex.oob[['x']]) | pv.all.0[2,] > max(hex.oob[['y']])
#
# # remaining oob triangle.
# # p coordinates of points to compute bcs on
# # v coordinates of corresponding triangles
#
# v <- rbind(
#   as.matrix(subset(hex.oob[1:7,], x > 0 & y > 0)),
#   vapply(hex.oob[1:7,], max, 1)
# )
# p <- t(pv.all.0[1:2,])
# pv.oob <- pv.oob | rowSums(bary_M(p, v) > 0) == 3
pv.oob <- pv.all[3,] > -.05

cyl.rad <- .035
pv.all.obj <- dplyr::bind_rows(
  lapply(
    seq_len(pv.all.n)[!pv.oob],
    function(i) {
      cylinder(
        x=pv.all[1,i], y=pv.all[2,i] - cyl.rad/3, z=pv.all[3,i],
        length=paver.w * .9, radius=cyl.rad,
        material=diffuse(color=pv.colors[i]),
        angle=c(90, row.ang[(i - 1)%/%15 + 1], 90),
        order_rotation=c(3, 1, 2),
        phi_min=20, phi_max=180
) } ) )
# - Portal ---------------------------------------------------------------------

message("portal")

# hex3 <- hex + .5
# hex3[8,] <- NA
# polypath(hex3, rule='evenodd', col='green')

# All a bit confusing because we do the extrusion in x-z plane, but then rotate.
# Should have done everything directly in x-y plane...
#
# Originally this was mean to fit exactly so the bag would be hidden by the hex
# logo rim, but now we let it spread wider since we added a wall.

h2 <- hex[1:7,]
h2.b <- back_hex(h2, obs * .3, -int[3,4] * 1.1)
bag.mat <- diffuse(color='#010102')
bag.mat <- diffuse(color='black')
bag <- comp_inside(h2, h2.b, bag.mat, light_index=6)

# Let's add a wall with a hex hole in it to look through.

portal <- rbind(
  matrix(c(-5,5, 5,5, 5,-5, -5,-5, -5,5), ncol=2, byrow=TRUE),
  as.matrix(unname(h2)),
  NULL
)
portal.rr1 <- extruded_polygon(
  portal, holes=c(6), material=diffuse('white'), top=0.0001, bottom=0.0001,
  angle=c(180,0,0)
)
portal.rr2 <- extruded_polygon(
  portal, holes=c(6), material=diffuse('white'), top=0, flip_vertical=TRUE
)
objs <- dplyr::bind_rows(
  lapply(
    letters,
    extrude_path, material=gold_mat, top=.025
  ),
  extrude_path(hex, material=diffuse('gray5'), top=.025),
  portal.rr1,
  # portal.rr2,
  bag,
  NULL
)
n <- 10

# - Process stars --------------------------------------------------------------

# compute star area

message("stars - basic")

star.a <- vapply(
  stars, function(x) {
    i <- seq_len(nrow(x) - 1L)
    ii <- seq_len(nrow(x) - 1L) + 1L   # i + 1
    sum(x[i,1] * x[ii,2] - x[ii,1] * x[i,2]) / 2
  },
  0
)
# Based on depth of nearest star, and relative sizes, compute:
# size of nearest star, distance of other stars.

star.widths <- vapply(
  stars, function(star) diff(range(star[['y']])), 0
)
buffer <- 1.1
# near <- obs[2] + brick.depth * buffer
near <- 3
star.z <- (obs[2] - (max(star.widths) / star.widths) * near)
star.x <- vapply(stars, function(x) mean(range(x[['x']])), 1)
star.y <- -vapply(stars, function(x) mean(range(x[['y']])), 1)
star.max <- which.max(star.widths)

# determine baseline dimensions of largest star (all others will be
# sized by distance)

star.scale <- (obs[2] - star.z) / obs[2]
star.dummy <- stars[[star.max]]
star.dummy[] <- lapply(
  star.dummy, function(x) (x - mean(range(x))) * star.scale[star.max]
)
# Split hex into triangles
# could have used extrude_polygon with no extrusion

coords <- decido::earcut(star.dummy)
tc <- split(cbind(star.dummy[coords,], z=0), rep(1:4, each=3))
tc <- lapply(tc, function(x) as.matrix(x))

make_star <- function(x, y, z, tc, angle=0, flip, color='#FFFFDD') {
  off <- c(x, y, z)
  raw <- lapply(
    1:4,
    function(i) {
      triangle(
        v1=tc[[i]][1,], v2=tc[[i]][2,], v3=tc[[i]][3,],
        material=diffuse(color), flipped=flip
  ) } )
  group_objects(
    dplyr::bind_rows(raw), group_translate=off, group_angle=c(0, angle, 0),
    pivot_point=numeric(3)
  )
}
# make original logo stars

stars.xyz <- rbind(star.x * star.scale, star.y * star.scale + .5, star.z)
#
# Want to add stars along path, maybe once we exit the original frame, so need
# to find where the road hits the frame.  At that point we'll add stars just at
# the edge of the FOV, so more or less constant radius from the road in the
# visible arc.

# All we care about is the x distance normalized for depth, find first out of
# frame point

oof <- match(
  TRUE, int.dots.3d[1,] / (obs[2] - int.dots.3d[3,]) * obs[2] < min(hex[,1])
)
# Want the stars filling the FOV from an observer at some point on the road,
# between x0 and oob (edge of frame seen from original pov), so closest point of
# unit square is computed based on fov.  We'll compute along vector to terminus

x0 <- sum(rle(sign(int.dots.3d[1,]))[['lengths']][1:2])
pov.x <- min(hex[,1]) / 2
pov <- int.dots.3d[,
  match(
    TRUE,
    int.dots.3d[1,] * obs[2] / (obs[2] - int.dots.3d[3,]) < pov.x &
    seq_len(ncol(int.dots.3d)) > x0
  )
]
# We're going to "interlock" cones of stars, overlapping a little bit.  We also
# need a wreath around the entrance so it we don't see the big gap when we
# start.  It could be a cone with the hex cut-out
#
# What's positive and negative is a bit of a mess.
#
# @param obs how far observer is from beginning of stars (should be positive)

message("stars - extra")

# Need to transition to layers

star_cone <- function(
  points, depth, layers, n, start, obs, mult=3, dmin=.04 * mult, 
  empty=.5 * mult
) {
  vetr::vetr(
    matrix(numeric(), 3) && ncol(.) > 2 && ncol(.) > start,
    NUM.1.POS, INT.1.POS.STR, INT.1.POS.STR, INT.1.POS.STR,
    NUM.1.POS, NUM.1.POS
  )
  # generate random x/y vals

  se.x <- se.y <- se.zi <- numeric(n)
  # * 20 may not be enough..., but also prevents infinite loop
  zoffs <- seq(0, depth, length.out=layers)
  xseed <- (runif(n * 20) - .5) * mult
  yseed <- (runif(n * 20) - .5) * mult
  zseedi <- sample(seq_along(zoffs), n * 20, replace=TRUE)
  zseed <- zoffs[zseedi]
  j <- 1
  dmin2 <- dmin^2

  for(i in seq_len(n)) {
    while(
      any(
        (se.x - xseed[j]) ^ 2 + (se.y - yseed[j]) ^ 2 < dmin2 |
        (
          (xseed[j] * (zseed[j] + obs)) ^ 2 +
          (yseed[j] * (zseed[j] + obs)) ^ 2 < empty
    ) ) )
      j <- j + 1
    se.x[i] <- xseed[j]
    se.y[i] <- yseed[j]
    se.zi[i] <- zseedi[j]
    j <- j + 1
  }
  # figure out where layers will be

  points <- points[,seq(start, ncol(points), 1L)]
  ds <- cumsum(sqrt(colSums((points[,-1] - points[,-ncol(points)])^2)))
  if(max(ds) < depth) {
    # not enough points, extend last vector until enough
    vec.last <- points[, ncol(points)] - points[, ncol(points) - 1]
    extra <- ceiling((depth - max(ds)) / sqrt(sum(vec.last ^ 2)))
    points <- cbind(
      points,
      matrix(vec.last * rep(seq_len(extra), each=3), 3) + points[, ncol(points)]
    )
  }
  ds <- cumsum(sqrt(colSums((points[,-1] - points[,-ncol(points)])^2)))
  ds.d <- match(TRUE, ds >= depth)
  ds.i <- seq_len(ds.d + 1)

  p1 <- interp_along(points[, ds.i], seq(.001, 1, length.out=layers))
  p0 <- interp_along(points[, ds.i], seq(0, .999, length.out=layers))
  vs <- p1 - p0
  vs <- vs / sqrt(colSums(vs^2))

  # Allocate points to layer

  l <- split(seq_len(n), se.zi)
  coords.l <- lapply(
    seq_along(l),
    function(i) {
      z <- zoffs[i]
      z.off <- obs + z
      l.c <- cbind(se.x[l[[i]]] / obs * z.off, se.y[l[[i]]] / obs * z.off, 0)

      # rotate onto the provided vector and offset
      # https://math.stackexchange.com/a/2672702 thanks Nico Schlomer

      a <- c(0, 0, -1)
      b <- vs[,i]
      R <- 2 * (a + b) %*% t(a + b) / c(t(a + b) %*% (a + b)) - diag(3)
      t(l.c %*% R) + p0[,i]
  } )
  list(coords=do.call(cbind, coords.l), end=start + ds.d)
}
ncone <- 6
cones <- vector('list', ncone)
start <- x0 + 1
for(i in seq_len(ncone)) {
  tmp <- star_cone(int.dots.3d, depth=10, 5, 50, start, 3, mult=3, empty=1.2)
  start <- tmp[['end']] + 1
  cones[[i]] <- tmp[['coords']]
}
# plot3d(t(cones[[1]]))
s.e.c <- do.call(cbind, cones)
# plot3d(
#   t(cbind(do.call(cbind, cones), int.dots.3d, stars.xyz)),
#   col=c(
#     rep(palette.colors(5, "R4"), each=50), rep('black', ncol(int.dots.3d)),
#     rep('red', ncol(stars.xyz))
#   )
# )
# compute framing stars which basically should be right outside the hexagon
# between 0 + half star-width and `near`

star.width <- diff(range(do.call(rbind, tc)[,1]))
frame.start <- -near
mult <- 5
set.seed(2)
star.frame.raw <- star_cone(
  rbind(0, 0, seq(frame.start, -near*3, length.out=4)),
  depth=near*3 + frame.start,
  n=600, layers=5, start=1, 
  obs=obsz[3] - frame.start, mult=mult, dmin=.03 * mult
)[['coords']]
hex.oob <- hex[1:7,] * .92
v <- rbind(
  as.matrix(subset(hex.oob, x > 0 & y > 0)),
  vapply(hex.oob, max, 1)
)
star.frame.xy <- star.frame.raw[1:2,] /
  rep((obsz[3] - star.frame.raw[3,]), each=2)
star.oob <-
  star.frame.xy[1,] > max(hex.oob[,1]) |
  star.frame.xy[1,] < min(hex.oob[,1]) |
  star.frame.xy[2,] > max(hex.oob[,2]) |
  star.frame.xy[2,] < min(hex.oob[,2]) |
  # a bit fidly, need to make sure the edge of a star not visible so
  # don't allow coords very close to vertices, not formally correct
  rowSums(bary_M(t(abs(star.frame.xy)), v) > .00) == 3

star.frame <- star.frame.raw[, star.oob] + c(0, .5, 0)
stars.all <- cbind(stars.xyz, star.frame, s.e.c)
# stars.all <- star.frame

tmp <- t(star.frame.xy[1:2, ])
# tmp <- t(star.frame.xy[1:2,])
plot(
  rbind(as.matrix(hex.oob), tmp), 
  col=c(rep('red', nrow(hex.oob)), rep('black', nrow(tmp)))
)
lines(hex.oob, col='green')

# plot3d(
#   t(cbind(int.dots.3d[,1:50], stars.xyz, star.frame)),
#   col=c(
#     rep('black', 50),
#     rep('gray', ncol(stars.xyz)), rep('red', ncol(star.frame))
#   )
# )

# - Castle ---------------------------------------------------------------------

# See castle.R

c.r <- 1
c.xyz <- int.dots.3d[,ncol(int.dots.3d)]
c.v <- c.xyz - int.dots.3d[,ncol(int.dots.3d) - 1]
c.v <- c.v / sqrt(sum(c.v^2))
c.xyz <- c.xyz + c.v * c.r

castle <- sphere(
  x=c.xyz[1], y=c.xyz[2], z=c.xyz[3], radius=c.r, material=light(intensity=2)
)

# - Containers -----------------------------------------------------------------

# Need a sphere that will contain everything, a box for the `rlang` world.

buff <- 1.01
rad <- max(sqrt(rowSums(h2.b^2))) * buff
bubble <- sphere(radius=rad, material=diffuse(), flipped=TRUE)

# - Render! --------------------------------------------------------------------

# Make a path starting from obs and following the path.  A bit complicated
# because we have to join smoothly to the point at x == 0 in the path.
#
# Find first second zero point after the road starts curving to the left.

x01m <- abs(int.dots.3d[1,x0] / diff(int.dots.3d[1,x0 + 0:1]))
dot0 <- int.dots.3d[,x0] + x01m * (int.dots.3d[,x0+1] - int.dots.3d[,x0])

# Need smooth curvature in y and x from starting obs to the x == 0 zero point.

obs2 <- .75 * obs[2]  # the first step will be straight from obs to obs2
slope0 <- 0
slopey1 <- diff(int.dots.3d[2,x0+0:1])/diff(int.dots.3d[3,x0+1:0])
slopex1 <- diff(int.dots.3d[1,x0+0:1])/diff(int.dots.3d[3,x0+1:0])
sfuny <- splinefunH(c(0, obs2 - dot0[3]), c(0, dot0[2]), c(slope0, slopey1))
sfunx <- splinefunH(c(0, obs2 - dot0[3]), c(0, dot0[1]), c(slope0, slopex1))
z2 <- seq(0, obs2 - dot0[3], length.out=10)
ys <- sfuny(z2)
xs <- sfunx(z2)
zs <- obs2 - z2

path.start <- cbind(obsz, rbind(xs, ys, zs))
path.all <- cbind(path.start, int.dots.3d[,-(seq_len(x0))])

# .5-1 second fade from white
# .5 second pause
# 1 second stars spin
# 2 seconds into portal
# 2 seconds in transit
# 2 seconds into castle and fade to white

frames <- 10
coast <- 2/6
frames.start <- frames.end <- (frames * (1 - coast)) %/% 2
frames.coast <- frames - 2 * frames.start
coast.point <- .05
frame.points <- c(
  seq(0, 1, length.out=frames.start)^3 * coast.point,
  seq(
    coast.point, 1 - coast.point, length.out=frames.coast + 2
  )[-c(1L, frames.coast + 2L)],
  1 - rev(seq(0, 1, length.out=frames.start)^3 * coast.point)
)
path.int <- interp_along(path.all, frame.points)

l.r <- rad / 8
l.d <- rad * .4
l.b <- 8
x0c <- int.dots.3d[,x0]
oofc <- int.dots.3d[,oof]

stars.xz <- rbind(stars.all[1,], 0, stars.all[3,])
star.v0 <-  (stars.xz - obsz) / rep(sqrt(colSums((stars.xz - obsz)^2)), each=3)

# give angle a direction based on the y value of the cross product

star.angle.0 <- acos(colSums(star.v0 * -obsz)) / pi * 180 *
  sign(xprod(star.v0, -obsz)[2,])

star.meta <- rbind(
  speed=pmax(0, rnorm(ncol(star.v0), 45, 30)),
  angle=ifelse(
    seq_len(ncol(star.v0)) > ncol(stars.xyz) + ncol(star.frame),
    star.angle.0 + 90, 0
  )
)
duration <- 1   # in seconds
# duration <- .0001   # in seconds
tmp <- vector('list', ncol(path.int)-1)

for(i in seq(1, ncol(path.int)-1, by=1)) {
  time <- duration * (i - 1) / (ncol(path.int) - 2)
  i <- 1
  a <- path.int[, i]
  b <- path.int[, i+1]
  lf <- a + c(0, .5, 0)
  la <- b + c(0, .5, 0)
  lv <- (la - lf) / sqrt(sum((la - lf)^2))

  star.v0 <-  (stars.xz - lf) / rep(sqrt(colSums((stars.xz - lf)^2)), each=3)
  star.angle <- acos(colSums(star.v0 * lv)) / pi * 180 *
    sign(xprod(star.v0, lv)[2,])

  # stars that are part of the second set should start at 90 degrees from the
  # angle.  So the angle about the y axis should be start angle + 90 degrees.

  stars <- mapply(
    make_star, stars.all[1,], stars.all[2,], stars.all[3,],
    star.meta['angle', ],
    flip=(abs(star.meta['angle', ] - star.angle) > 90),
    MoreArgs=list(tc=tc), SIMPLIFY=FALSE
  )
  # stars <- mapply(
  #   make_star, star.frame[1,], star.frame[2,], star.frame[3,],
  #   numeric(ncol(star.frame)),
  #   flip=logical(ncol(star.frame)),
  #   MoreArgs=list(tc=tc), SIMPLIFY=FALSE
  # )
  scene <- dplyr::bind_rows(
    group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
    pv.all.obj,
    bubble,
    sphere(z=l.d, x=l.d, radius=l.r, material=light(intensity=l.b)),
    sphere(z=l.d, x=-l.d, radius=l.r, material=light(intensity=l.b)),
    stars,
    castle
  )
  render_scene(
    scene,
    filename=next_file("~/Downloads/rlang/video/img-"),
    lookfrom=lf, 
    # lookat=la,
    lookat=c(0.1, .78, 0),
    # lookfrom=c(20, .5, 2), lookat=c(0, .5, 0),
    # width=720, height=720, samples=200,
    samples=25,
    clamp_value=5,
    # fov=fov,        # this affects computations above
    fov=10,
    aperture=0
  )
  star.meta['angle',] <- star.meta['angle',] + star.meta['speed',] * time
  tmp[[i]] <- star.meta['angle',] + star.meta['speed',] * time
}

# objs <- dplyr::bind_rows(
#   Map(
#     extrude_path, rev(paths), top=c(.1, .05) ,
#     bottom=c(-.1,-.05), material=list(gold_mat)
#   )
# )


# krender_scene(
# k  cylinder(
# k    angle=c(90,0,-90), phi_min=0, phi_max=180, material=diffuse('red'),
# k    order_rotation=c(3,2,1)
# k  ),
# k  lookfrom=c(5,0,5),
# k  fov=30
# k)
