source('static/script/_lib/rayrender.R')
library(rayrender)
library(ambient)    # for water patterns

eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))
der <- elmat1[-1,]
sea <- 50                 # sea level

# - Depth ----------------------------------------------------------------------

# For each depth = 0 point we want to find distane to the nearest ground point
# to use that as depth.  We're going to do brute force that by breaking up the
# map into smaller pieces.  This is not a general solution but will do for us.
# Could generate 75e6 nrow matrices with the current problem size (derwent)

# Take 2, find all the edges by looking at the 4 squares around each land square
# and see if there is any water adjacent

# stop()
# land <- der > sea
# idx <- as.matrix(subset(expand.grid(x=-1:1, y=-1:1), xor(x, y)))
# landi <- which(land, arr.ind=TRUE)
# idx2 <- landi[rep(seq_len(nrow(landi)), each=nrow(idx)), ] +
#   do.call(rbind, replicate(nrow(landi), idx, simplify=FALSE))
#
# # Use NAs to track oob, we'll make them land later
# idx2[idx2 == 0L] <- NA
# idx2[idx2[,1] > nrow(land), 1] <- NA
# idx2[idx2[,2] > ncol(land), 2] <- NA
# land.adj <- matrix(land[idx2], nrow(idx))
# land.adj[is.na(land.adj)] <- TRUE
#
# # Compute which lands are adjacent to water
# land.adj.water <- which(colSums(land.adj) < nrow(idx))
#
# # Too many, so sample randomly a subset that should be good enough
# set.seed(123)
# land.adj.w2 <- sample(land.adj.water, length(land.adj.water) / 2)
#
# lc <- t(landi[land.adj.w2,])
# wc <- t(which(!land, arr.ind=TRUE))
# wlcc <- expand.grid(w=seq_len(ncol(wc)), l=seq_len(ncol(lc)))
# wldist <- colSums((wc[, wlcc[['w']]] - lc[, wlcc[['l']]]) ^ 2)
# dim(wldist) <- c(ncol(wc), ncol(lc))
# nearl <- max.col(-wldist, ties.method='first')
# depth <- array(NA_real_, dim=dim(der))
# depth.dat <- sqrt(colSums((wc - lc[,nearl]) ^ 2))
# depth[t(wc)] <- (depth.dat - min(depth.dat)) / diff(range(depth.dat))
# saveRDS(depth, '~/Downloads/derwent/depth2.RDS')
#
der2 <- der - sea
depth2 <- readRDS('~/Downloads/derwent/depth2.RDS')
depth.vals <- sqrt(depth2[!is.na(depth2)]) * (max(der2)) / 2

# - Mesh -----------------------------------------------------------------------

library(rtini)
tol <- 10

# Force the water boundary to break to smallest tris
der2[!is.na(depth2)] <- -depth.vals - (tol * 1.1)
err <- rtini_error(der2)
# plot(as.raster((der2 - min(der2))/diff(range(der2)))

# Add back the tolerance, but then a little step so water boundary at sharper
# angle

der2[!is.na(depth2)] <- der2[!is.na(depth2)] + (tol * 1.1) - 5
ids <- rtini_extract(err, tol=tol)

tris <- rbind(do.call(cbind, ids), NA)
# plot.new()
# polygon(
#   x=((tris - 1) %/% nrow(err)) / (ncol(err) - 1),
#   y=((tris - 1) %% nrow(err)) / (nrow(err) - 1),
#   col='grey90'
# )

source('static/script/mesh-viz/viz-lib.R')
xyz <- ids_to_xyz(ids, der2, scale=NULL)
xyz2 <- xyz
max.xy <- max(unlist(xyz[c('x', 'y')]))
xyz$x <- (xyz$x - mean(range(xyz$x))) / max.xy
xyz$y <- (xyz$y - mean(range(xyz$y))) / max.xy
xyz$z <- xyz$z / max(der) / 3

# Make sure these are all counterclockwise by checking triangle area sign.
# Assumes nothing steeper than vertical.

i <- 1:3
ii <- c(2:3,1)
base <- matrix(rep(seq(0, length(xyz[[1]]) - 1, by=3), each=3), 3)
iv <- i + base
iiv <- ii + base
area_s <- with(xyz, colSums(matrix((x[iv] * y[iiv] - x[iiv] * y[iv]) / 2, 3)))
ccw <- area_s >= 0  # treat degenerates as counter-clockwise
if(!all(ccw)) stop('bad triangle winding')

# Add color depending on Z value
pos.col <- c(col2rgb('lightgreen') / 255)
neg.col <- c(col2rgb('grey95') / 255)
mesh0 <- xyz_to_mesh(xyz)
mesh1 <- mesh_skirt(mesh0, vcolor=NULL)
mesh.col <- matrix(
  unlist(
    lapply(
      mesh1[, 'z'],
      function(x) {
        res <- matrix(numeric(), 3, length(x))
        res[, x > 0] <- pos.col
        res[, x < 0] <- neg.col
        asplit(res, 1)
      }
    ),
    recursive=FALSE
  ),
  nrow=3,
  byrow=TRUE
)
mesh <- cbind(mesh1, mesh.col)

obj <- mesh_to_obj(mesh)
f <- tempfile()
writeLines(obj, f)

# - Scene & Render -------------------------------------------------------------

xang <- 80
xw <- diff(range(xyz$x)) * .999
zw <- .999
ymin <- min(unlist(mesh[, 'z']))

# - Water ----------------------------------------------------------------------
# Adapted from:
# https://gist.github.com/tylermorganwall/7f31a10f22dc5912cc86b8b312f6f335

cxy <- expand.grid(x=seq_len(nrow(der)),y=seq_len(ncol(der)))
steps <- 1
for(i in seq(0, 360, length.out=steps + 1)[-(steps + 1)]) {
  ang <- i
  writeLines(sprintf('Frame %d %s', i, Sys.time()))
  #i <- ang <- angs[j]
  fbase <- .025
  amb <- gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase, seed = 1) +
    gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase * 2, seed = 2)/2 +
    gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase * 4, seed = 3)/4 +
    0
  amb <- (amb - min(amb)) / diff(range(amb))

  att <- c(1,.7,0.3) * 4
  water <- dielectric(
    # color='#E0F5FF',
    refraction=1.33,
    bump_texture=matrix(amb, nrow(der), ncol(der)),
    # bump_intensity=6,
    bump_intensity=3,
    attenuation = att
  )
  water2 <- dielectric(refraction=1.33, attenuation = att)

  water.obj <- group_objects(
    dplyr::bind_rows(
      xz_rect(material=water,  xwidth=xw, y=0, zwidth=zw),
      xz_rect(material=water2, xwidth=xw, y=ymin, flipped=TRUE, zwidth=zw),
      xy_rect(
        material=water2, xwidth=xw, y=ymin/2, z=-zw/2, ywidth=-ymin, flipped=TRUE
      ),
      xy_rect(material=water2, xwidth=xw, y=ymin/2, z=zw/2,  ywidth=-ymin),
      yz_rect(
        material=water2, y=ymin/2, x=xw/2,  ywidth=-ymin, zwidth=zw,
        flipped=TRUE
      ),
      yz_rect(material=water2, y=ymin/2, x=-xw/2, ywidth=-ymin, zwidth=zw),
    ),
    pivot_point=numeric(3),
    group_angle=c(0, ang, 0)
  )
  # water.obj <- cube(
  #   xwidth=xw, zwidth=zw, ywidth=-ymin, y=ymin/2, angle=c(0, ang, 0),
  #   material=diffuse('lightblue')
  # )
  scene <- dplyr::bind_rows(
    sphere(y=5, z=3, x=2, radius=1, material=light(intensity=30)),
    group_objects(
      obj_model(f, vertex_colors=TRUE),
      pivot_point=numeric(3), group_angle=c(90, ang, 180),
      group_order_rotation=c(3, 1, 2),
    ),
    water.obj,
    xz_rect(
      xwidth=6, zwidth=6, y=6,
      material=diffuse('lightblue'), flipped=TRUE,
      angle=c(25, 0, 0)
    ),
    # generate_studio(
    #   width=20, height=20,
    #   distance=-5, curvature=1, depth=-2,
    #   # material=light(intensity=.975)
    #   # material=diffuse(checkercolor='gray50', checkerperiod=.25)
    # )
  )
  dmult <- 4
  render_scene(
    scene,
    # fov=75 / dmult,
    fov=80,
    # width=1200, height=800, samples=200,
    width=400, height=400, samples=25,
    # lookat=c(0, 0, 0.15),
    # lookfrom=c(0, 0.5, 0.8) * dmult,
    lookfrom=c(.15, 0.01, .3),          # on lake
    # lookfrom=c(0, 0, 10),
    aperture=0,
    clamp_value=5,
    # ambient_light=TRUE,
    # debug_channel='normals',
    filename=next_file('~/Downloads/derwent/vmov-1/img-001.png')
    # filename=next_file('~/Downloads/derwent/v1/img-000.png')
  )
}
stop()

# - Perlin Experiments ---------------------------------------------------------

steps <- 360
angs <- seq(0, 360, length.out=steps + 1)
hzs <- c(.2, .1, .05, .025)
for(j in seq_len(steps)) {
  png(next_file('~/Downloads/derwent/perlin3/img-000.png'))
  par(mai=numeric(4))
  i <- ang <- angs[j]
  cxy <- expand.grid(x=seq_len(nrow(der)),y=seq_len(ncol(der)))
  # fbase <- 0.05
  # fbase <- hzs[j]
  fbase <- .025
  amb <- gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase, seed = 1) +
    gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase * 2, seed = 2)/2 +
    gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase * 4, seed = 3)/4 +
    0

  amb <- (amb - min(amb)) / diff(range(amb))
  plot(as.raster(array(amb, dim=dim(der))))
  dev.off()
}
stop()
x <- list.files('~/Downloads/derwent/perlin3', pattern='^img', full.names=TRUE)
file.copy(
  x[c(-1,-length(x))],
  file.path(
    dirname(x[[1]]),
    sprintf("img-%03d.png", rev(seq_len(length(x) - 2) + length(x)))
  )
)


  file.path('~/Downloads/derwent/perlin3', x[1:30]),
  '~/Downloads/derwent/perlin2'
)
a <- list.files('~/Downloads/derwent/perlin2', pattern='^img', full.names=TRUE)[-c(1,30)]
b <- file.path(
  '~/Downloads/derwent/perlin2',

)
file.copy(a, b)
