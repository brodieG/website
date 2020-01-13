# just the errors

source('static/script/mesh-viz/viz-batch-init.R')

# errs2a.cyl <- errs_to_cyl(
#   transform(errs2.df, z0=0, x=x-.5, y=y-.5),
#   diffuse(color='grey75', checkercolor='grey35', checkerperiod=.05)
# )
# errs3a.cyl <- errs_to_cyl(
#   transform(errs3.df, z0=0, x=x-.5, y=y-.5),
#   diffuse(color=metal.col[3], checkercolor='grey75', checkerperiod=.05)
# )
# 
# # Just the errors
# 
# # cmat <- diffuse(color='#CCCCCC')
# cmat <- diffuse(color='white')
# ang1 <- c(-90, 0, 0)
# ang2 <- c(-90, 90, 0)
# crad <- .025
# grid <- dplyr::bind_rows(
#   cylinder(x=-.25, y=0, length=1.5, radius=crad, material=cmat),
#   cylinder(x=-.75, y=0, length=1.5, radius=crad, material=cmat),
#   cylinder(x=+.25, y=0, length=1.5, radius=crad, material=cmat),
#   cylinder(x=+.75, y=0, length=1.5, radius=crad, material=cmat)
# )
# wl <- .4
# scn.4 <- dplyr::bind_rows(
#   scn.base,
#   group_objects(grid, group_angle=c(-90, 0, 0)),
#   group_objects(grid, group_angle=c(-90, 90, 0)),
#   group_objects(
#     errs2a.cyl, group_angle=gang,
#     pivot_point=numeric(3)
#   ),
#   group_objects(
#     errs3a.cyl, group_angle=gang,
#     pivot_point=numeric(3)
#   ),
#   cube(
#     xwidth=1.75, zwidth=1.75, ywidth=wl,
#     material=dielectric(color='#BBBBCC'),
#     y=wl / 2 + .025000001
#   ),
#   # sphere(material=diffuse(color='green'), radius=.1)
#   NULL
# )
# bg1 <- 102
# # bg1 <- 80
# bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
# rez <- 400
# samp <- 200
# # scns <- list(scn.1, scn.2, scn.3)
# scns <- list(scn.4)
# render_scenes(
#   scns, height=rez, width=rez, samples=samp,
#   lookfrom=c(0, 4, 1),
#   # lookfrom=c(0, 2, 2),
#   lookat=c(0, 0, 0),
#   # fov=50,
#   fov=25,
#   aperture=0,
#   camera_up=c(0,1,0),
#   clamp=3,
#   backgroundlow=bg, backgroundhigh=bg,
#   ambient_light=TRUE,
#   filename='~/Downloads/mesh-viz/small-mesh/simple-mesh-s2b-%d.png'
# )
# stop('done')

source('static/script/mesh-viz/viz-waves.R')
# f1 <- tempfile()
# f2 <- tempfile()
# f3 <- tempfile()

water_surface(f1, 50)
water_surface(f2, 50, t=pi/10)
water_surface(f3, 50, t=pi/5)

# par3d(windowRect=c(0, 0, 600, 600))
# plot3d(readOBJ(f), color='grey')

# mat.w <- dielectric(color='#F0F0FF', refraction=1.3)
# mat.w <- dielectric(color='#E5E5FF', refraction=1.3)
mat.w <- dielectric(color='#E0F5FF', refraction=1.3)
dy1 <- .15
dy2 <- .4
dy3 <- .525
objrr3 <- obj_model(f3, material=mat.w, scale=rep(1.21/(2*pi), 3), y=dy3)
objrr2 <- obj_model(f2, material=mat.w, scale=rep(1.21/(2*pi), 3), y=dy2)
objrr1 <- obj_model(f1, material=mat.w, scale=rep(1.21/(2*pi), 3), y=dy1)

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
dro <- .09
dri <- .07
dyy1 <- dy1 - 0.003
dyy2 <- dy2 - 0.003
dyy3 <- dy3 - 0.003
# mat.d <- diffuse(color='#55CC55')
mat.d <- diffuse(color='grey10')
scn.4a <- dplyr::bind_rows(
  scn.base.2, objrr1,
  disk(radius=dro, inner_radius=dri, y=dyy1, z=-.5, material=mat.d),
  disk(radius=dro, inner_radius=dri, y=dyy1, x=-.5, material=mat.d),
  disk(radius=dro, inner_radius=dri, y=dyy1, material=mat.d)
)
scn.4b <- dplyr::bind_rows(
  scn.base.2, objrr2,
  disk(radius=dro, inner_radius=dri, y=dyy2, x=-.5, material=mat.d),
  disk(radius=dro, inner_radius=dri, y=dyy2, material=mat.d)
)
scn.4c <- dplyr::bind_rows(
  scn.base.2, objrr3,
  disk(radius=dro, inner_radius=dri, y=dyy3, x=-.5, material=mat.d),
)

# rez <- 600
# samp <- 500
rez <- 300
samp <- 25
# rez <- 300
# samp <- 100

# scns <- list(scn.4a, scn.4b, scn.4c)
# scns <- list(scn.4b)
file <- next_file('~/Downloads/mesh-viz/small-mesh/simple-mesh-new2-')
# render_scene(
render_scene(
  scn.4c,
  # list(scn.4a, scn.4b, scn.4c),
  # filename='~/Downloads/mesh-viz/small-mesh/water-new7f-%d.png',
  filename=file,
  height=rez, width=rez, samples=samp,
  # lookfrom=c(0, 3, 1.5), lookat=c(0, 0, 0),
  lookfrom=c(0, 3, 1.5), 
  lookat=c(0, .25, 0), fov=27,
  # lookat=c(0, .25, 0), fov=9,
  # lookat=c(-.25, .5, 0), fov=15,
  # lookfrom=c(2, 1, 2), lookat=c(0, 0, 0),
  # fov=10,
  # fov=0, ortho_dimensions=c(1.75,1.75),
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE
)
stop()

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
  # lookat=c(0, 0, 0), fov=25,
  lookat=c(-.25, 0, 0), fov=25,
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
