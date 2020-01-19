# just the errors

source('static/script/mesh-viz/viz-batch-init.R')

# these start on the ground

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

source('static/script/mesh-viz/viz-waves.R')
source('static/script/mesh-viz/viz-batch-init.R')

# f1 <- tempfile()
# f2 <- tempfile()
# f3 <- tempfile()
f4 <- tempfile()

water_surface(f1, 150)
water_surface(f2, 150, t=pi/10)
water_surface(f3, 150, t=pi/5)
water_surface(f4, 150, t=pi/5 + pi/10)

# par3d(windowRect=c(0, 0, 600, 600))
# plot3d(readOBJ(f), color='grey')

# mat.w <- dielectric(color='#F0F0FF', refraction=1.3)
# mat.w <- dielectric(color='#E5E5FF', refraction=1.3)
mat.w <- dielectric(color='#E0F5FF', refraction=1.3)
dy1 <- .15
dy2 <- .275
dy3 <- .525
dy4 <- .525
objrr4 <- obj_model(f4, material=mat.w, scale=rep(1.21/(2*pi), 3), y=dy3)
objrr3 <- obj_model(f3, material=mat.w, scale=rep(1.21/(2*pi), 3), y=dy3)
objrr2 <- obj_model(f2, material=mat.w, scale=rep(1.21/(2*pi), 3), y=dy2)
objrr1 <- obj_model(f1, material=mat.w, scale=rep(1.21/(2*pi), 3), y=dy1)

rad <- .05
dots <- base_points(
  which(errs1 == 0, arr.ind=TRUE), n=nrow(errs1), mat=dot.mat, rad=rad/3
)

gang <- c(90, 0, 0)
scn.base.2 <- dplyr::bind_rows(
  scn.base,
  # dots,
  group_objects(
    errs2b.cyl, group_angle=gang,
    pivot_point=numeric(3), 
  ),
  group_objects(
    errs3b.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  NULL
)
dyy1 <- dy1 - 0.02
dyy2 <- dy2 - 0.02
dyy3 <- dy3 - 0.02

# mat.d <- diffuse(color='#55CC55')
scn.4a <- dplyr::bind_rows(
  scn.base.2, objrr1,
  group_objects(d1, group_translate=c(0,dyy1,0)),
  group_objects(d2, group_translate=c(0,dyy1,0)),
  group_objects(d3, group_translate=c(0,dyy1,0)),
)
scn.4b <- dplyr::bind_rows(
  scn.base.2, objrr2,
  group_objects(d1, group_translate=c(0,dyy2,0)),
  group_objects(d2, group_translate=c(0,dyy2,0)),
)
scn.4c <- dplyr::bind_rows(
  scn.base.2, objrr3,
  group_objects(d1, group_translate=c(0,dyy3,0)),
)
scn.4d <- dplyr::bind_rows(
  scn.base.2, objrr4,
  group_objects(d1, group_translate=c(0,dyy3,0)),
  group_objects(d2, group_translate=c(0,dyy3,0)),
  cylinder(y=.45 + .25/2, length=.25, radius=0.05, material=silv.mat),
  disk(y=.7, radius=0.05, material=silv.mat)
)

# rez <- 600
# samp <- 500
rez <- 600
samp <- 400
# rez <- 300
# samp <- 25

# scns <- list(scn.4a, scn.4b, scn.4c)
# scns <- list(scn.4b)
# file <- next_file('~/Downloads/mesh-viz/small-mesh/simple-mesh-new2-')
# render_scene(
  # scn.4c,
  # filename=file,
render_scenes(
  # list(scn.4a, scn.4b, scn.4c),
  list(scn.4b, scn.4d),
  # list(scn.4a),
  filename='~/Downloads/mesh-viz/small-mesh/water-tmp2-%d.png',
  height=rez, width=rez, samples=samp,
  # lookfrom=c(0, 3, 1.5), lookat=c(0, 0, 0),
  lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=28,
  # lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=5,
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
# stop()
# 
# 
# # tar.dir <- '~/Downloads/mesh-viz/small-mesh/test'
# # xx <- list.files(tar.dir, full.names=TRUE, pattern=())
# # xx <- xx[c(4:6, 1:3)]
# # pngs <- lapply(xx, png::readPNG)
# # dim <- dim(pngs[[1]])
# # # res <- array(0, dim * c(3,2,1))
# # res <- array(0, dim * c(2,3,1))
# # for(i in seq_along(pngs)) {
# #   coff <- ((i - 1) %% 3) * dim[2]
# #   roff <- ((i - 1) %/% 3) * dim[1]
# #   # res[seq_len(dim[1]) + coff, seq_len(dim[2]) + roff,] <- pngs[[i]]
# #   res[seq_len(dim[1]) + roff, seq_len(dim[2]) + coff,] <- pngs[[i]]
# # }
# # png::writePNG(res, file.path(tar.dir, '_res2.png'))
# 
# res <- replicate(3, array(0, dim * c(1,2,1)), simplify=FALSE)
# for(i in  1:3) {
#   res[[i]][seq_len(dim[1]), seq_len(dim[2]),] <- pngs[[i]]
#   res[[i]][seq_len(dim[1]), seq_len(dim[2]) + dim[1],] <- pngs[[i + 3]]
#   png::writePNG(res[[i]], file.path(tar.dir, sprintf('_waterlev%d.png', i)))
# }
# 
# 
