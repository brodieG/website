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
rx %*% int

# test without interpolation

int.test <- dplyr::bind_rows(
  lapply(
    seq_len(ncol(int)),
    function(i)
      sphere(
        x=int[1,i], y=int[2,i], z=int[3,i],
        radius=.2, material=diffuse(color='green')
      )
  )
)
# int.test.r <- t(int.test) %*% rx

# rotate on x-axis

# Interpolate and rotate back

n.rows <- 400
int.dots <- bezier_interp_even(list(x=int.rot[,1], y=int.rot[,3]), n.rows)
int.dots.3d <- rx %*% rbind(int.dots[[1]], 0, int.dots[[2]])

# Place each rung in between the points, compute angle from vector

int.diff <- int.dots.3d[,-1] - int.dots.3d[,-ncol(int.dots.3d)]
int.diff.n <- int.diff / rep(sqrt(colSums(int.diff ^ 2)), each=3)
int.mid <- int.dots.3d[,-ncol(int.dots.3d)] + int.diff/2

int.test <- dplyr::bind_rows(
  lapply(
    seq_len(ncol(int)),
    function(i)
      sphere(
        x=int[1,i], y=int[2,i], z=int[3,i],
        radius=.4, material=diffuse(color='green')
      )
  )
)
set.seed(1)

# a row of pavers

off.raw <- rnorm(n.rows - 1) * .025         # noise in row position
row.ang <- -asin(int.diff.n[1,])/pi*180     # angle in x/z
row.ang2 <- -asin(int.diff.n[1,])/pi*180 - 90

off.x <- -off.raw * int.diff.n[3,]
off.z <- off.raw * int.diff.n[1,]

# sum(int.diff.n[,1] * vec) == 0
# a[1] * b[1] + a[2] * b[2] == 0

paver.n <- 15
paver.w <- 1/paver.n
paver.x <- seq(-.5 + paver.w / 2, .5 - paver.w / 2, length.out=paver.n)

make_paver_row <- function() {
  dplyr::bind_rows(
    lapply(
      paver.x,
      function(x) {
        cylinder(
          x=x, length=paver.w * .9, radius=.025,
          material=diffuse(
            rgb(sample(135:185, 1), sample(45:75, 1), 0, maxColorValue=255)
          ),
          angle=c(0, 0, 90)
) } ) ) }
# and a road of pavers

paver.off <- seq(-.5, .5, length.out=paver.n + 1)

int.diff.obj <- dplyr::bind_rows(
  lapply(
    seq_len(ncol(int.diff)),
    function(i) {
      group_objects(
        make_paver_row(),
        group_translate=c(
          int.mid[1,i] + off.x[i],
          int.mid[2,i],
          int.mid[3,i] + off.z[i]
        ),
        group_angle=c(0, row.ang[i], 0)
      )
    }
  )
)
int.obj <- dplyr::bind_rows(
  lapply(
    seq_len(ncol(int.dots.3d)),
    function(i)
      sphere(
        x=int.dots.3d[1,i], y=int.dots.3d[2,i], z=int.dots.3d[3,i],
        radius=.1, material=diffuse(color='red')
      )
  )
)
bg <- '#FFFFFF'
render_scene(
  dplyr::bind_rows(
    group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
    # b.p.obj,
    # int.obj,
    int.diff.obj,
    # int.test,
    sphere(z=15, y=6, x=15, radius=6, material=light(intensity=3)),
    sphere(z=-15, y=6, x=-15, radius=6, material=light(intensity=10)),
    sphere(radius=36, material=diffuse(), flipped=TRUE),
    sphere(z=-15, y=6, x=15, radius=6, material=light(intensity=10)),
  ),
  filename=next_file("~/Downloads/rlang/imgs/img-"),
  lookfrom=c(0, .5, 1), lookat=c(0, .5, 0),
  # lookfrom=c(0, 12, -1), lookat=c(-3, .5, -5),
  # lookfrom=c(0, .5, 5), lookat=c(0, .5, 0),
  # width=600, height=600, samples=100,
  samples=5,
  clamp_value=5,
  fov=60,
  # fov=90,
  aperture=0
)
# objs <- dplyr::bind_rows(
#   Map(
#     extrude_path, rev(paths), top=c(.1, .05) ,
#     bottom=c(-.1,-.05), material=list(gold_mat)
#   )
# )

