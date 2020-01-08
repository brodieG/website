# just the errors

source('static/script/mesh-viz/viz-batch-init.R')

errs2a.cyl <- errs_to_cyl(
  transform(errs2.df, z0=0, x=x-.5, y=y-.5),
  diffuse(color='grey50', checkercolor='grey25', checkerperiod=.05)
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

df <- expand.grid(x=seq(-pi,pi, pi/25), y=seq(-pi,pi, pi/25))
df <- transform(
  df, z=sin(2*x + 2 * y) + sin(2*x - 1.5 * y) +
    .2 * sin(10*x + 6 * y) + .3 * sin(10*y)
)
library(ggplot2)
ggplot(df, aes(x=x, y=y, fill=z)) + geom_raster() + scale_fill_viridis_c()

# f <- tempfile()
mesh <- mesh_tri(transform(df, y=z*.01, z=y, t=0), c(51, 51))

# Need to find all the vertices on the edges and generate a border out of them

x.lo <- min(unlist(mesh[,'x']))
x.hi <- max(unlist(mesh[,'x']))
z.lo <- min(unlist(mesh[,'z']))
z.hi <- max(unlist(mesh[,'z']))
y.lo <- min(unlist(mesh[,'y'])) - .25

xu <- unlist(mesh[,'x'])
zu <- unlist(mesh[,'z'])
yu <- unlist(mesh[,'y'])

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
bot <- matrix(
  list(
    c(x.lo, x.lo), c(x.lo, x.hi), c(x.hi, x.hi),
    c(y.lo, y.lo), c(y.lo, y.lo), c(y.lo, y.lo),
    c(z.lo, z.lo), c(z.hi, z.lo), c(z.hi, z.hi)
  ), 3
)
mesh2 <- mesh[,1:3]
mesh2[] <- Map(c, mesh2, mzx.lo, mzx.hi, mxz.lo, mxz.hi, bot)
mesh3 <- mesh2

obj <- mesh_to_obj(mesh3)
writeLines(obj, f)
plot3d(readOBJ(f), color='grey')

objrr <- obj_model(f, material=diffuse(color='#BBBBCC'))

scn.5 <- dplyr::bind_rows(
  light.narrow,
  objrr,
  NULL
)
rez <- 200
samp <- 50

scns <- list(scn.5)
render_scenes(
  scns, height=rez, width=rez, samples=samp,
  lookfrom=c(0, 3, 3),
  # lookfrom=c(0, 2, 2),
  fov=60,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE,
  filename='~/Downloads/mesh-viz/small-mesh/simple-mesh-s2c-%d.png'
)
# unlink(f)
