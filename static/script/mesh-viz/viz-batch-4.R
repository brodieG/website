# just the errors

source('static/script/mesh-viz/viz-batch-init.R')

errs2a.cyl <- errs_to_cyl(
  transform(errs2.df, z0=0, x=x-.5, y=y-.5),
  diffuse(color='grey75', checkercolor='grey35', checkerperiod=.05)
)
errs3a.cyl <- errs_to_cyl(
  transform(errs3.df, z0=0, x=x-.5, y=y-.5),
  diffuse(color=metal.col[3], checkercolor='grey75', checkerperiod=.05)
)

# Just the errors

# cmat <- diffuse(color='#CCCCCC')
cmat <- diffuse(color='white')
ang1 <- c(-90, 0, 0)
ang2 <- c(-90, 90, 0)
crad <- .025
grid <- dplyr::bind_rows(
  cylinder(x=-.25, y=0, length=1.5, radius=crad, material=cmat),
  cylinder(x=-.75, y=0, length=1.5, radius=crad, material=cmat),
  cylinder(x=+.25, y=0, length=1.5, radius=crad, material=cmat),
  cylinder(x=+.75, y=0, length=1.5, radius=crad, material=cmat)
)
wl <- .4
scn.4 <- dplyr::bind_rows(
  scn.base,
  group_objects(grid, group_angle=c(-90, 0, 0)),
  group_objects(grid, group_angle=c(-90, 90, 0)),
  group_objects(
    errs2a.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  group_objects(
    errs3a.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  cube(
    xwidth=1.75, zwidth=1.75, ywidth=wl,
    material=dielectric(color='#BBBBCC'),
    y=wl / 2 + .025000001
  ),
  # sphere(material=diffuse(color='green'), radius=.1)
  NULL
)
bg1 <- 102
# bg1 <- 80
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
rez <- 400
samp <- 200
# scns <- list(scn.1, scn.2, scn.3)
scns <- list(scn.4)
render_scenes(
  scns, height=rez, width=rez, samples=samp,
  lookfrom=c(0, 4, 1),
  # lookfrom=c(0, 2, 2),
  lookat=c(0, 0, 0),
  # fov=50,
  fov=25,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE,
  filename='~/Downloads/mesh-viz/small-mesh/simple-mesh-s2b-%d.png'
)
stop('done')

n <- 50
seqv <- seq(-pi,pi, length.out=n + 1)
df <- expand.grid(x=seqv, y=seqv)
hz <- 1
# df <- transform(
#   df, z=
#     1 * sin(2*x*hz + 4 * y*hz) +
#     .8 * sin(2*x*hz - 3 * y*hz) +
#     .4 * sin(10*x*hz + 6 * y* hz)
# )
t <- 0
# df <- transform(
#   df, z=
#     .125 * sin(2*((x + pi)*hz)^2 + 4*(y*hz)^2) +
#     .10 * sin(3*((x - pi)*hz)^2 + 2*((y + pi)*hz)^2) +
#     .125 * sin(3*((x - 2 *pi)*hz)^2 + 6*((y + pi)*hz)^2)
# )
df <- transform(
  df, z=
    .2 * sin(2*(x + t)*hz + 4 * (y + t)*hz) +
    .125 * sin(2*((x + t + pi)*hz)^2 + 4*((y + t)*hz)^2) +
    .10 * sin(3*((x - pi + t)*hz)^2 + 2*((y + pi + t)*hz)^2) +
    .125 * sin(3*((x - 2 *pi + t)*hz)^2 + 6*((y + pi + t)*hz)^2)
)
df <- transform(
  df, z=
    .5 * sin(2*(x+t)*hz + 4 * (y+t)*hz) +
    .125 * sin(2*((x+t + 1.5*pi)*hz)^2 + 4*((y+t)*hz)^2) +
    .125 * sin(3*((x+t - pi/2)*hz)^2 + 2*((y + t + 2*pi)*hz)^2) +
    .3 * sin(2*(x+t)*hz - 3 * (y+t)*hz)
  )
df <- transform(
  df, z=
    .5 * sin(2*(x+t)*hz + 4 * (y+t)*hz) +
    .125 * sin(2*(((x+t) + pi)*hz)^2 + 4*((y+t)*hz)^2) +
    .125 * sin(3*(((x+t) + pi/4)*hz)^2 + 2*(((y+t) + pi)*hz)^2) +
    .3 * sin(2*(x+t)*hz - 3 * (y+t)*hz)
  )
# df <- transform(df, z=sin(2*x + 4 * y))
library(ggplot2)
ggplot(df, aes(x=x, y=-y, fill=z)) + geom_raster() + scale_fill_viridis_c()

# f <- tempfile()
mesh <- shadow::mesh_tri(transform(df, y=z*.05, z=y), c(n, n)+1)
m1 <- mesh
m1[] <- lapply(mesh, '[', 1)

# Need to find all the vertices on the edges and generate a border out of them

xu <- unlist(mesh[,'x'])
zu <- unlist(mesh[,'z'])
yu <- unlist(mesh[,'y'])

x.lo <- min(xu)
x.hi <- max(xu)
z.lo <- min(zu)
z.hi <- max(zu)
y.lo <- -10
if(y.lo > min(yu)) stop('need lower y')

vertex_arrange <-function(a, b, y, lo) {
  which.a <- b == lo
  base.a <- a[which.a]
  o <- order(base.a)
  ao <- base.a[o]
  d <- duplicated(ao)
  aou <- ao[!d]
  you <- y[which.a][o][!d]
  list(aou, you)
}
zx.lo <- vertex_arrange(zu, xu, yu, x.lo)
zx.hi <- vertex_arrange(zu, xu, yu, x.hi)
xz.lo <- vertex_arrange(xu, zu, yu, z.lo)
xz.hi <- vertex_arrange(xu, zu, yu, z.hi)

to_mesh <- function(v, w, y, y.lo) {
  lv <- length(v)
  y.lo.r <- rep(y.lo, lv - 1)

  L <- matrix(
    list(
      c(v[-lv], v[-1]), c(v[-lv], v[-1]), c(v[-1], v[-lv]),
      c(y[-lv], y.lo.r), c(y.lo.r, y[-1]),  c(y[-1], y.lo.r),
      rep(w, (lv - 1) * 2), rep(w, (lv - 1) * 2), rep(w, (lv - 1) * 2)
    ),
    3
  )
}
mzx.lo <- to_mesh(zx.lo[[1]], x.lo, zx.lo[[2]], y.lo)[,3:1]
mzx.hi <- to_mesh(zx.hi[[1]], x.hi, zx.hi[[2]], y.lo)[,3:1]
mxz.lo <- to_mesh(xz.lo[[1]], z.lo, xz.lo[[2]], y.lo)
mxz.hi <- to_mesh(xz.hi[[1]], z.hi, xz.hi[[2]], y.lo)
# bot <- matrix(
#   list(
#     c(x.lo, x.lo), c(x.hi, x.hi), c(x.hi, x.lo),
#     c(y.lo, y.lo), c(y.lo, y.lo), c(y.lo, y.lo),
#     c(z.lo, z.lo), c(z.lo, z.hi), c(z.hi, z.hi)
#   ), 3
# )
mesh2 <- mesh[,1:3]
# mesh2[] <- Map(c, mesh2, mzx.lo, mzx.hi, mxz.lo, mxz.hi, bot)
mesh2[] <- Map(c, mesh2, mzx.lo, mzx.hi, mxz.lo, mxz.hi)
mesh3 <- mesh2

# obj <- mesh_to_obj(mesh3, c(0, y.lo * .98, 0))
obj <- mesh_to_obj(mesh3, c(0, -5, 0))
# obj <- mesh_to_obj(mesh3)
writeLines(obj, f)
# par3d(windowRect=c(0, 0, 600, 600))
# plot3d(readOBJ(f), color='grey')

# mat.w <- dielectric(color='#F0F0FF', refraction=1.3)
# mat.w <- dielectric(color='#E5E5FF', refraction=1.3)
mat.w <- dielectric(color='#E0F5FF', refraction=1.3)
objrr3 <- obj_model(f, material=mat.w, scale=rep(1.5/(2*pi), 3), y=.525)
objrr2 <- obj_model(f, material=mat.w, scale=rep(1.5/(2*pi), 3), y=.4)
objrr1 <- obj_model(f, material=mat.w, scale=rep(1.5/(2*pi), 3), y=.2)

mult <- 1.3
light.narrow <- sphere(
  y=8, z = 2, x = 1, radius = .5,
  material = light(intensity = 200 * mult)
)
bg1 <- 102 / mult
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
scn.base <- dplyr::bind_rows(
  light.narrow,
  # xz_rect(
  #   xwidth=15, zwidth=15, y=10, flipped=TRUE,
  #   material=diffuse(color='white', lightintensity=1)
  # )
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white'))
)
gang <- c(90, 0, 0)
scn.base.2 <- dplyr::bind_rows(
  scn.base,
  group_objects(
    errs2a.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  group_objects(
    errs3a.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  NULL
)
dy2 <- .400
dy1 <- .200
dy3 <- .600
dro <- .10
dri <- .075
# mat.d <- diffuse(color='#55CC55')
mat.d <- diffuse(color='black')
scn.4a <- dplyr::bind_rows(
  scn.base.2, objrr1,
  disk(radius=dro, inner_radius=dri, y=dy1, z=-.5, material=mat.d),
  disk(radius=dro, inner_radius=dri, y=dy1, x=-.5, material=mat.d),
  disk(radius=dro, inner_radius=dri, y=dy1, material=mat.d)
)
scn.4b <- dplyr::bind_rows(
  scn.base.2, objrr2,
  disk(radius=dro, inner_radius=dri, y=dy2, x=-.5, material=mat.d),
  disk(radius=dro, inner_radius=dri, y=dy2, material=mat.d)
)
scn.4c <- dplyr::bind_rows(
  scn.base.2, objrr3,
  disk(radius=dro, inner_radius=dri, y=dy3, x=-.5, material=mat.d),
)

# rez <- 800
# samp <- 300
rez <- 300
samp <- 150
# rez <- 100
# samp <- 25

file <- next_file('~/Downloads/mesh-viz/small-mesh/simple-mesh-new2-')
# scns <- list(scn.4a, scn.4b, scn.4c)
# scns <- list(scn.4b)
# render_scene(
render_scenes(
  list(scn.4a, scn.4b, scn.4c),
  height=rez, width=rez, samples=samp,
  lookfrom=c(0, 3, 1.5), lookat=c(0, 0, 0),
  # lookfrom=c(0, 4, 1), lookat=c(0, 0, 0),
  # fov=25,
  fov=35,
  # fov=0, ortho_dimensions=c(1.75,1.75),
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE,
  # filename=file
  filename='~/Downloads/mesh-viz/small-mesh/water-new5-%d.png'
)

rez <- 200
samp <- 250
mat.w <- dielectric(color='#FFBBBB')
mat.w <- dielectric(color='#FFBBBB')
ang <- c(0, 0, 0)

scn.base <- dplyr::bind_rows(
  sphere(material=light(intensity=100), y=5, x=0, z=0),
  xz_rect(
    y=-.49, xwidth=15, zwidth=15,
    material=diffuse(checkercolor='green', checkerperiod=.25)
  ),
  cube(material=mat.w, x=.75, angle=ang)
)
scn <- group_objects(
  dplyr::bind_rows(
    # xz_rect(material=mat.w, y=-.5, flipped=FALSE),
    xz_rect(material=mat.w, y=.5),
    xy_rect(material=mat.w, z=-.5, flipped=TRUE),
    xy_rect(material=mat.w, z=.5),
    yz_rect(material=mat.w, x=-.5, flipped=TRUE),
    yz_rect(material=mat.w, x=.5)
  ),
  group_translate=c(-.75,0,0),
  group_angle=ang
)
render_scene(
  dplyr::bind_rows(scn.base, scn),
  height=rez, width=rez, samples=samp,
  lookfrom=c(0, 2, 7),
  # lookfrom=c(-7, 4, 0),
  lookat=c(0, 0, 0),
  fov=25,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  # ambient_light=TRUE,
  filename='~/Downloads/mesh-viz/small-mesh/cube-1.png'
)

# unlink(f)
m1 <- m5001 <- m5101 <- mesh
m1[] <- lapply(mesh, '[', 1)
m5001[] <- lapply(mesh, '[', 5001)
m5101[] <- lapply(mesh, '[', 5101)
#
