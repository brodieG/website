source('static/script/mesh-viz/viz-lib.R')

library(svgchop)
# url <- 'https://www.r-project.org/logo/Rlogo.svg'
url <- '~/Downloads/rlang/rlang2.svg'

library(rayrender)
gold_mat <- microfacet(
  roughness=0.1, eta=c(0.216,0.42833,1.3184), kappa=c(3.239,2.4599,1.8661)
)
gold <- "#BB5500"
zz <- parse_paths(url)
ww <- interp_paths(zz, normalize=TRUE)
paths <- lapply(
  ww,
  function(x) {
    res <- x[['d']]
    res[[1]] <- res[[1]] - .5
    res[[2]] <- res[[2]] - .5
    res
  }
)
extrude_path <- function(x, ...) {
  extruded_polygon(
    x, holes=if(length(attr(x, 'starts'))) attr(x, 'starts'), ...
  )
}

# Generate the Rlang pocket.  Want two sets of hex coordinates, the front, back,
# and want to generate triangles for the back and sides.  Need to compute from
# our viewpoint and project out the back hexagon.
#
# Challenge, compute what elements in our full scene are actually inside.
#
# @param depth how far from the hex edge to the corresponding vertex in the back
#   hexagon.
# @param y numeric(1L) y coord of original hexagon parallel to x-z plane
# @param obs the observer coordinates (x,y,z)
# @param hex the coordinates of the hex, hex assumed closed, first coordinate is
#   taken to be X, second Z, and the Y value is assumed to be 0.

comp_inside <- function(
  hex, y, obs, depth, material=diffuse(color='red'),
  light_index=1
) {
  vetr::vetr(
    structure(list(numeric(7), numeric(7)), class='data.frame'),
    numeric(1),
    numeric(3),
    numeric(1)
  )
  hex.in <- rbind(hex[[1]], 0, hex[[2]])
  vecs <- hex.in - obs
  vecs.n <- vecs / sqrt(colSums(vecs^2)) * depth
  hex.out <- hex.in + vecs.n

  i <- seq_len(ncol(vecs.n) - 1L)
  ii <- seq_len(ncol(vecs.n) - 1L) + 1L   # i + 1
  area_s <- sum(hex.in[1,i] * hex.in[3,ii] - hex.in[1,ii] * hex.in[3,i]) / 2

  ccw <- area_s >= 0  # treat degenerates as counter-clockwise
  flip <- !ccw

  # code adapted from rayrender

  sides <- unlist(
    lapply(
      seq_len(ncol(vecs.n) - 1L),
      function(i) {
        if(i == light_index) material=light(intensity=5)
        list(
          triangle(
            v1=hex.in[,i], v2=hex.out[,i], v3=hex.out[,i+1],
            material = material, reversed = flip
          ),
          triangle(
            v1=hex.in[,i], v2=hex.out[,i+1], v3=hex.in[,i+1],
            material = material, reversed = flip
          )
    ) } ),
    recursive=FALSE
  )
  back.idx <- matrix(decido::earcut(hex), 3)
  back.tris <- lapply(
    split(back.idx, col(back.idx)),
    function(i)
      triangle(
        hex.out[,i[1]], hex.out[,i[2]], hex.out[,i[3]],
        material=material
      )
  )
  dplyr::bind_rows(c(sides, back.tris))
}
# Hex is a mess, we need to clean it up

hex <- paths[[length(paths)]]
outer <- c(3, 6, 7, 10, 13, 14, 3)
hex <- hex[c(outer, 17:nrow(hex)),]
attr(hex, 'starts') <- 8

# All a bit confusing because we do the extrusion in x-z plane, but then rotate.
# Should have done everything directly in x-y plane...

h2 <- hex[1:7,]
brick.depth <- 10
depth <- 20
obs <- c(0, 1, 0)
bag <- comp_inside(
  h2, 0, obs, depth=depth, diffuse(color='#010102'), light_index=6
)
objs <- dplyr::bind_rows(
  lapply(
    paths[-c(1, length(paths))],
    extrude_path, material=gold_mat, top=.025
  ),
  extrude_path(hex, material=diffuse('gray10'), top=.025),
  bag
)
n <- 10
brick.start <- 0
y.rise <- .6
angle <- atan(y.rise / (brick.depth + brick.start)) / pi * 180
h <- sqrt(y.rise^2 + (brick.depth + brick.start)^2)

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
brick.path.2[2:3, 2] <- c(.47)

p0 <- c(0, 0, 0)
p1 <- c(0, y.rise, -brick.depth)
p2 <- c(1, y.rise, -brick.depth)
p01 <- p1 - p0
p02 <- p2 - p0
la <- obs[c(1,3,2)] + c(0,.5,0)
lab <- t(as.matrix(transform(brick.path.2, z=0))) - la

p01xp02 <- c(
   (p01[2] * p02[3] - p01[3] * p02[2]),
  -(p01[1] * p02[3] - p01[3] * p02[1]),
   (p01[1] * p02[2] - p01[2] * p02[1])
)
t <- sum(p01xp02 * (la - p0)) /
     colSums(-lab * p01xp02)
int <- la + lab * rep(t, each=3)

ang.rad <- angle / 180 * pi
rx <- matrix(
  c(1,0,0, 0,cos(ang.rad),sin(ang.rad), 0,-sin(ang.rad),cos(ang.rad)), 3
)
int.rot <- t(int) %*% rx

# Interpolate and rotate back

n.rows <- 600
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

pv.all.mult <- obs[2] / (obs[2] - pv.all[3,])
pv.all.0 <- pv.all
pv.all.0[1:2,] <- 
  (pv.all.0[1:2,] - c(0, .5)) * rep(pv.all.mult, each=2)
pv.all.save <- pv.all.0

pv.all.0[1:2,] <- abs(pv.all.0[1:2,])

# oob bounding box
pv.oob <- pv.all.0[1,] > max(hex[['x']]) | pv.all.0[2,] > max(hex[['y']])

# remaining oob triangle.
# p coordinates of points to compute bcs on
# v coordinates of corresponding triangles

bary_M <- function(p, v) {
  det <- (v[2,2]-v[3,2])*(v[1,1]-v[3,1]) +
         (v[3,1]-v[2,1])*(v[1,2]-v[3,2])

  l1 <- (
          (v[2,2]-v[3,2]) * (p[,1]-v[3,1]) +
          (v[3,1]-v[2,1]) * (p[,2]-v[3,2])
        ) / det
  l2 <- (
          (v[3,2]-v[1,2]) * (p[,1]-v[3,1]) +
          (v[1,1]-v[3,1]) * (p[,2]-v[3,2])
        ) / det
  l3 <- 1 - l1 - l2
  cbind(l1, l2, l3)
}
v <- rbind(
  as.matrix(subset(hex[1:7,], x > 0 & y > 0)),
  vapply(hex[1:7,], max, 1)
)
p <- t(pv.all.0[1:2,])
pv.oob <- pv.oob | rowSums(bary_M(p, v) > 0) == 3

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
# Hexagons

coords <- decido::earcut(hex[1:7,])
tc <- split(cbind(hex[1:7,][coords,], z=0), rep(1:4, each=3))
tc <- lapply(tc, function(x) as.matrix(x)/2)

make_star <- function(x, y, z) {
  off <- c(x, y, z)
  lapply(
    1:4,
    function(i) {
      triangle(
        v1=tc[[i]][1,] + off, v2=tc[[i]][2,] + off, v3=tc[[i]][3,] + off,
        material=diffuse(gold)
      )
    }
  )
}
nstars <- 200
z <- runif(nstars, -20, -10)
x <- runif(nstars, -.5, .5) * (1 - z) / 1.25
y <- runif(nstars, -.5, .5) * (1 - z) / 1.25

stars <- Map(make_star, x, y, z)

bg <- '#FFFFFF'
render_scene(
  dplyr::bind_rows(
    group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
    pv.all.obj,
    # sphere(z=15, y=6, x=15, radius=6, material=light(intensity=3)),
    # sphere(z=-15, y=6, x=-15, radius=6, material=light(intensity=10)),
    # sphere(radius=36, material=diffuse(), flipped=TRUE),
    # sphere(z=-15, y=6, x=15, radius=6, material=light(intensity=10)),
    sphere(z=15, y=6, x=15, radius=6, material=light(intensity=3)),
    sphere(z=-15, y=6, x=-15, radius=6, material=light(intensity=10)),
    sphere(radius=72, material=diffuse(), flipped=TRUE),
    sphere(z=-15, y=6, x=15, radius=6, material=light(intensity=10)),
    unlist(stars, recursive=FALSE),
  ),
  filename=next_file("~/Downloads/rlang/imgs/img-"),
  lookfrom=c(0, .5, 1), lookat=c(0, .5, 0),
  # lookfrom=c(0, .5, .25), lookat=c(-1, .6, -2),
  # lookfrom=c(0, 12, -1), lookat=c(-3, .5, -5),
  # lookfrom=c(0, .5, 5), lookat=c(0, .5, 0),
  width=600, height=600, samples=200,
  # samples=10,
  clamp_value=5,
  fov=60,
  # fov=15,
  aperture=0
)
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
