source('static/script/_lib/rayrender.R')
eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))
der <- elmat1[-1,]
sea <- 50                 # sea level

# par(mai=numeric(4), mfrow=c(2,2))
# plot(as.raster(der <= 0))
# plot(as.raster(der <= 5))
# plot(as.raster(der <= 10))
# plot(as.raster(der <= 20))


# - Depth ----------------------------------------------------------------------

# For each depth = 0 point we want to find distane to the nearest ground point
# to use that as depth.  We're going to do brute force that by breaking up the
# map into smaller pieces.  This is not a general solution but will do for us. 
# Could generate 75e6 nrow matrices with the current problem size (derwent)

# divs <- 4
# rows <- split(
#   seq_len(nrow(der)), as.integer((seq_len(nrow(der)) - 1) / nrow(der) * divs)
# )
# cols <- split(
#   seq_len(ncol(der)), as.integer((seq_len(ncol(der)) - 1) / ncol(der) * divs)
# )
# lots <- expand.grid(r=seq_along(rows), c=seq_along(cols))
# depth <- array(NA_real_, dim(der))
# 
# for(i in seq_len(nrow(lots))) {
#   xs <- rows[[lots[i, 'r']]]
#   ys <- cols[[lots[i, 'c']]]
#   lot <- der[xs, ys]
#   zero <- lot < sea
#   if(all(zero)) {
#     warning('bad tiling for tile ', i)
#   } else if (!any(zero)) {
#   } else {
#     wat <- which(zero)
#     land <- which(!zero)
#     writeLines(
#       sprintf(
#         "working on tile %d land: %d water: %d", i, length(land), length(wat)
#       )
#     )
#     watc <- rbind(
#       xs[(wat - 1) %% nrow(lot) + 1], 
#       ys[(wat - 1) %/% nrow(lot) + 1]
#     )
#     landc <- rbind(
#       xs[(land - 1) %% nrow(lot) + 1], 
#       ys[(land - 1) %/% nrow(lot) + 1]
#     )
# 
#     # brute force every water to every land coordinate
#     wlcc <- expand.grid(w=seq_len(ncol(watc)), l=seq_len(ncol(landc)))
#     wldist <- colSums((watc[, wlcc[['w']]] - landc[, wlcc[['l']]]) ^ 2)
# 
#     # for each water, find index of nearest land
#     dim(wldist) <- c(ncol(watc), ncol(landc))
#     nearl <- max.col(-wldist, ties.method='first')
#     depth[t(watc)] <- sqrt(colSums((watc - landc[,nearl]) ^ 2))
#   }
# }
# depth2 <- depth
# depth2 <- (depth - min(depth, na.rm=TRUE)) / diff(range(depth, na.rm=TRUE))
# saveRDS(depth2, '~/Downloads/derwent/depth.RDS')
#
#
der2 <- der
der2 <- der - sea
depth2 <- readRDS('~/Downloads/derwent/depth.RDS')
depth.vals <- sqrt(depth2[!is.na(depth2)]) * (max(der2)) / 3

# - Water ----------------------------------------------------------------------
# Adapted from:
# https://gist.github.com/tylermorganwall/7f31a10f22dc5912cc86b8b312f6f335

library(rayrender)
library(ambient)

i <- 1
cxy <- expand.grid(x=seq_len(nrow(der)),y=seq_len(ncol(der)))
amb <- gen_simplex(cxy[,1],cxy[,2],z=i/30,frequency = 0.1,seed = 1) +
  gen_simplex(cxy[,1],cxy[,2],z=i/30,frequency = 0.1*2,seed = 2)/2 +
  gen_simplex(cxy[,1],cxy[,2],z=i/30,frequency = 0.1*4,seed = 3)/4 
amb <- (amb - min(amb)) / diff(range(amb))
water <- dielectric(
  # color='#E0F5FF', 
  refraction=1.33,
  bump_texture=matrix(amb, nrow(der), ncol(der)),
  attenuation = c(1,1,0.3)/2
)
# water <- dielectric(
#   attenuation = c(1,1,0.3)/2, refraction=1.33,
#   # color='#E0F5FF'
# )

# - Mesh -----------------------------------------------------------------------

library(rtini)
tol <- 10

# Force the water boundary to break to smallest tris
der2[!is.na(depth2)] <- -depth.vals - (tol * 1.1)
err <- rtini_error(der2)
# plot(as.raster((der2 - min(der2))/diff(range(der2)))

# Add back the tolerance, but then a little step so water boundary at sharper
# angle

der2[!is.na(depth2)] <- der2[!is.na(depth2)] + (tol * 1.1) - 20
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
xyz$z <- xyz$z / max(der) / 2

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

mesh <- mesh_skirt(xyz_to_mesh(xyz))
# mesh[,'z'] <- lapply(mesh[,'z'], '-', .5)
obj <- mesh_to_obj(mesh)
f <- tempfile()
writeLines(obj, f)

# - Scene & Render -------------------------------------------------------------

xang <- 80
scene <- dplyr::bind_rows(
  # sphere(y=5, z=5, x=2, radius=1, material=light(intensity=40)),
  sphere(y=5, z=5, x=-2, radius=1, material=light(intensity=40)),
  group_objects(
    obj_model(f, material=diffuse('lightgreen')),
    pivot_point=numeric(3), group_angle=c(xang, 0, 180), 
    group_order_rotation=c(3, 1, 2),
  ),
  # cube(y=-.5, material=diffuse(color='blue'), angle=c(xang - 90, 0, 0)),
  # group_objects(
  #   cube(
  #     y=-.5, xwidth=diff(range(xyz$x)) * .99, zwidth=.99, material=water,

  #   ), 
  #   pivot_point=c(0, 0, 0),
  #   group_angle=c(xang - 90, 0, 0),
  # ),
  # cube(y=(-1 * .95)/2, material=water, angle=c(xang - 90, 0, 0)),
  # cube(y=(-1 * .99)/2, material=water, angle=c(xang - 90, 0, 0)),
  generate_studio(
    material=light(intensity=.975), width=10, height=10,
    distance=-5, curvature=1, depth=-2
  )
)
render_preview(
  scene,
  fov=75,
  width=1200, height=800, samples=400,
  # width=400, height=400, samples=5,
  lookat=c(0, 0, 0),
  lookfrom=c(0, 0.15, .8),
  # lookfrom=c(3, 0, 0),
  aperture=0,
  clamp_value=5,
  # debug_channel='normals',
  filename=next_file('~/Downloads/derwent/v1/img-000.png')
)
