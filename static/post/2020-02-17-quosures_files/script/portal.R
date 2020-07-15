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

brick.depth <- 14
depth <- 20

# this is messed up, y and z are swapped due to original extrusion being on
# floor

obs <- c(0, 1, 0)

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

# Hex is a mess, we need to clean it up

hex <- frame[[1L]]
outer <- c(3, 6, 7, 10, 13, 14, 3)
hex <- hex[c(outer, 17:nrow(hex)),]
attr(hex, 'starts') <- 8

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
la <- obs[c(1,3,2)] + c(0,.5,0)
lab <- t(as.matrix(transform(brick.path.2, z=0))) - la

# cross prod: p01 x p02
p01xp02 <- c(
   (p01[2] * p02[3] - p01[3] * p02[2]),
  -(p01[1] * p02[3] - p01[3] * p02[1]),
   (p01[1] * p02[2] - p01[2] * p02[1])
)

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
# bary_M <- function(p, v) {
#   det <- (v[2,2]-v[3,2])*(v[1,1]-v[3,1]) +
#          (v[3,1]-v[2,1])*(v[1,2]-v[3,2])
# 
#   l1 <- (
#           (v[2,2]-v[3,2]) * (p[,1]-v[3,1]) +
#           (v[3,1]-v[2,1]) * (p[,2]-v[3,2])
#         ) / det
#   l2 <- (
#           (v[3,2]-v[1,2]) * (p[,1]-v[3,1]) +
#           (v[1,1]-v[3,1]) * (p[,2]-v[3,2])
#         ) / det
#   l3 <- 1 - l1 - l2
#   cbind(l1, l2, l3)
# }
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

# - Process stars --------------------------------------------------------------

# compute star area

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
near <- obs[2] + brick.depth * buffer
near <- .5
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

make_star <- function(x, y, z, scale, tc) {
  off <- c(x * scale, y * scale + .5, z)
  lapply(
    1:4,
    function(i) {
      triangle(
        v1=tc[[i]][1,] + off, v2=tc[[i]][2,] + off, v3=tc[[i]][3,] + off,
        material=diffuse('#FFFFDD')
) } ) }
stars.fin <- mapply(
  make_star, star.x, star.y, star.z, star.scale, MoreArgs=list(tc=tc),
  SIMPLIFY=FALSE
)
# - Castle ---------------------------------------------------------------------

# See castle.R

# - Containers -----------------------------------------------------------------

# Need a sphere that will contain everything, a box for the `rlang` world.

buff <- 1.01
rad <- max(sqrt(rowSums(h2.b^2))) * buff
bubble <- sphere(radius=rad, material=diffuse(), flipped=TRUE)

# - Render! --------------------------------------------------------------------

l.r <- rad / 8
l.d <- rad * .4
l.b <- 8
scene <- dplyr::bind_rows(
  group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
  pv.all.obj,
  bubble,
  sphere(z=l.d, x=l.d, radius=l.r, material=light(intensity=l.b)),
  sphere(z=l.d, x=-l.d, radius=l.r, material=light(intensity=l.b)),
  unlist(stars.fin, recursive=FALSE),
)
# Make a path starting from obs and following the path.  A bit complicated
# because we have to join smoothly to the point at x == 0 in the path.
#
# Find first zero point

x0 <- rle(sign(int.dots.3d[1,]))[['lengths']][1]
x01m <- abs(int.dots.3d[1,x0] / diff(int.dots.3d[1,x0 + 0:1]))
dot0 <- int.dots.3d[,x0] + x01m * (int.dots.3d[,x0+1] - int.dots.3d[,x0])

# Need smooth curvature in y from starting obs to the zero point.

slope0 <- 0
slope1 <- diff(int.dots.3d[2,x0+0:1])
sfun <- splinefunH(c(0, 1 - dot0[3]), c(0, dot0[2]), c(slope0, slope1))
z2 <- seq(0, 1 - dot0[3], length.out=10)
ys <- sfun(z2)
zs <- 1 - z2

path.all <- cbind(
  rbind(0, ys, zs), int.dots.3d[,-(seq_len(x0))]
)
path.int <- interp_along(path.all, c(0, .005, .01, .015, .02, .05))

for(i in seq(1, ncol(path.int)-1, by=1)) {
  a <- path.int[, i]
  b <- path.int[, i+1]
  lf <- a + c(0, .5, 0)
  la <- b + c(0, .5, 0)

  render_scene(
    scene,
    filename=next_file("~/Downloads/rlang/imgs/img-"),
    lookfrom=lf, lookat=la,
    # lookfrom=c(20, .5, 2), lookat=c(0, .5, 0),
    # width=720, height=720, samples=25,
    samples=5,
    clamp_value=5,
    fov=60,
    # fov=20,
    aperture=0
  )
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
