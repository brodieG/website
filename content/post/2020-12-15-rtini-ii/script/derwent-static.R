library(rtini)
library(vetr)
library(ambient)
library(rayrender)
source('static/script/_lib/rayrender.R')
source('static/script/_lib/plot.R')

writeLines('generating data')

eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))
der <- elmat1[-1,]
sea <- 50                 # sea level

# To actually extract meshes with variable tolerances
source('content/post/2020-12-15-rtini-ii/script/mesh-build.R')

# see original derwent file for depth

der2 <- der - sea
depth2 <- readRDS('content/post/2020-12-15-rtini-ii/data/depth2.RDS')
depth.vals <- sqrt(depth2[!is.na(depth2)]) * (max(der2)) / 2

tol.cheat <- 200
der2[!is.na(depth2)] <- -depth.vals - (tol.cheat * 1.1)
err <- rtini_error(der2)
der2[!is.na(depth2)] <- der2[!is.na(depth2)] + (tol.cheat * 1.1) - 5

f <- tempfile()
mesh.dat <- build_der_mesh(der2, err, 5, file=f)

# generate large matrix
# map <- der2
# n <- 4
# for(i in seq_len(n)) {
#   map <- cbind(map, map[, rev(seq_len(ncol(map)))])
#   map <- rbind(map, map[rev(seq_len(nrow(map))),])
# }
# # Need a few more cols to get to 8192 x 8193
# map <- cbind(map[, rev(seq_len(200))], map)
# k <- 1025
# k2 <- 4097
# m <- map[seq_len(k), seq_len(k)]
# m2 <- map[seq_len(k2), seq_len(k2)]
# m3 <- map[seq_len((k2 - 1)/2+1), seq_len(k2)]

m <- der2
err <- rtini_error(m)

mesh.dir <- tempfile()  # remember to unlink this
dir.create(mesh.dir)

# mesh.ll <- build_der_mesh(m, err, 100, file=file.path(mesh.dir, "mesh-lo.obj"))
# mesh.lo <- build_der_mesh(m, err, 10, file=file.path(mesh.dir, "mesh-lo.obj"))
# mesh.hi <- build_der_mesh(m, err, 1, file=file.path(mesh.dir, "mesh-hi.obj"))
# mesh.kal <- build_der_mesh(
#   m2, err2, 100, file=file.path(mesh.dir, "mesh-kal.obj"), height.scale=6
# )
# mesh.kal.w <- build_der_mesh(
#   m3, err3, 100, file=file.path(mesh.dir, "mesh-kal-w.obj"), height.scale=6
# )

xw <- diff(range(mesh.dat$xyz$x)) * .999
zw <- diff(range(mesh.dat$xyz$y)) * .999
ymin <- min(mesh.dat$xyz$z)
cxy <- expand.grid(x=seq_len(nrow(m)),y=seq_len(ncol(m)))

i <- 1
j <- i
ti <- i

angs <- 0
angsc <- 0
ang <- angs[i]
angc <- angsc[i]
steps <- 1:360
# angle to use for wave pattern, offset to an angle were we won't notice the
# repeat happening (i.e. when looking far from water / behind mountains).
writeLines(sprintf('Frame %d %s', i, Sys.time()))

# Water, adapted from:
# https://gist.github.com/tylermorganwall/7f31a10f22dc5912cc86b8b312f6f335
fbase <- .025
amb <- gen_simplex(cxy[,1], cxy[,2], z=angc, frequency = fbase, seed = 1) +
  gen_simplex(cxy[,1], cxy[,2], z=angc, frequency = fbase * 2, seed = 2) / 2 +
  gen_simplex(cxy[,1], cxy[,2], z=angc, frequency = fbase * 4, seed = 3) / 4
amb <- (amb - min(amb)) / diff(range(amb))
att <- c(1,.7,0.3) * 4
ref <- 1.33
water <- dielectric( # color='#E0F5FF',
  refraction=ref,
  bump_texture=matrix(amb, nrow(m), ncol(m)),
  bump_intensity=3,
  attenuation=att
)
water2 <- dielectric(refraction=ref, attenuation = att)
# water <- water2 <- diffuse(color='#E0F5FF')
# water3 <- diffuse(color='#E0F5FF')

water.obj <- group_objects(
  dplyr::bind_rows(
    xz_rect(material=water,  xwidth=xw, y=0, zwidth=zw),
    xz_rect(material=water2, xwidth=xw, y=ymin, flipped=TRUE, zwidth=zw),
    xy_rect(
      material=water2, xwidth=xw, y=ymin/2, z=-zw/2, ywidth=-ymin,
      flipped=TRUE
    ),
    xy_rect(material=water2, xwidth=xw, y=ymin/2, z=zw/2,  ywidth=-ymin),
    yz_rect(
      material=water2, y=ymin/2, x=xw/2,  ywidth=-ymin, zwidth=zw,
    ),
    yz_rect(
      material=water2, y=ymin/2, x=-xw/2, ywidth=-ymin, zwidth=zw,
      flipped=TRUE
    ),
  ),
  pivot_point=numeric(3),
  group_angle=c(0, ang, 0)
)
# Assemble scene
scene <- dplyr::bind_rows(
  sphere(y=5, z=1, x=1, radius=.5, material=light(intensity=125)),
  group_objects(
    obj_model(f, vertex_colors=TRUE),
    pivot_point=numeric(3), group_angle=c(90, 0, 180),
    group_order_rotation=c(3, 1, 2),
  ),
  water.obj,
  # "sky" reflector
  xz_rect(
    xwidth=50, zwidth=50, y=6,
    # material=diffuse(), flipped=TRUE,
    # material=diffuse('deepskyblue'), 
    material=light(intensity=.35),
    flipped=TRUE,
    # angle=c(25, 0, 0)
  ),
  xz_rect(xwidth=10, zwidth=10, y=ymin * 1.01)
  # cube(scale=.2, y=.3),
  # generate_studio(
  #   material=light(intensity=1, importance_sample=FALSE),
  #   width=8, height=6, distance=-5
  # )
)
obs.off <- -.022 * c(1, 0, 1)
render_scene(
  scene,
  # fov=0,
  fov=22.5,
  # width=800, height=800, samples=100,
  width=1200, height=1200, samples=800,
  # width=720, height=720, samples=100,
  # width=600, height=600, samples=20,
  # width=400, height=400, samples=50,
  # width=800, height=800, samples=300,
  # width=400, height=400, samples=200,
  # lookat=la[,i],
  lookat=c(0, 0, 0) + obs.off,
  # lookfrom=c(0, 3.8, 0.000001) + obs.off,
  lookfrom=c(0, 2, 2) + obs.off, 
  aperture=0,
  clamp_value=5,
  # debug_channel='normals',
  # filename=sprintf('D:Downloads/rtini/v5/img-%03d.png', i)
  filename=next_file('~/Downloads/derwent/v5/img-001.png')
)
stop()

source('static/script/_lib/png.R')
dir <- '~/Downloads/derwent/v5/'
input <- file.path(dir, c('fov-single-lo.png', 'fov-single-hi.png'))
cbind_pngs(input, file.path(dir, 'fov-twin.png'))


fov.lo <- png::readPNG()
fov.hi <- png::readPNG('~/Downloads/derwent/v5/fov-single-hi.png')



