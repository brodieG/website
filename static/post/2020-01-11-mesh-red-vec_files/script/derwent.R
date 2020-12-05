
eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))
der <- elmat1[-1,]


library(rtini)
err <- rtini_error(der)
ids <- rtini_extract(err, tol=100)
tris <- do.call(cbind, mesh)

source('static/script/mesh-viz/viz-lib.R')

xyz <- ids_to_xyz(ids, der, scale=c(1, 1, 1))

# Make sure these are all counterclockwise by checking triangle area sign.
# Assumes nothing steeper than vertical.

i <- 1:3
ii <- c(2:3,1) 
base <- matrix(rep(seq(0, length(xyz[[1]]), by=3), each=3), 3)
iv <- i + base
iiv <- ii + base
area_s = with(xyz, colSums(matrix(x[iv] * y[iiv] - x[iiv] * y[iv] / 2, 3)))
ccw = area_s >= 0  # treat degenerates as counter-clockwise
if(!all(ccw)) stop('bad triangle winding')

# - Water ----------------------------------------------------------------------
# Adapted from:
# https://gist.github.com/tylermorganwall/7f31a10f22dc5912cc86b8b312f6f335

mesh <- xyz_to_mesh(xyz)
obj <- mesh_to_obj(mesh)

f <- tempfile()
writeLines(obj, f)

xang <- 80
library(rayrender)
library(ambient)

i <- 1
cxy <- expand.grid(x=seq_len(nrow(der)),y=seq_len(ncol(der)))
amb <- gen_simplex(cxy[,1],cxy[,2],z=i/30,frequency = 0.1,seed = 1) +
  gen_simplex(cxy[,1],cxy[,2],z=i/30,frequency = 0.1*2,seed = 2)/2 +
  gen_simplex(cxy[,1],cxy[,2],z=i/30,frequency = 0.1*4,seed = 3)/4 
amb <- (amb - min(amb)) / diff(range(amb))
water <- dielectric(
  # color='#E0F5FF', refraction=1.3,
  bump_texture=matrix(amb, nrow(der), ncol(der)),
  attenuation = c(1,1,0.3)/2
)
# - Scene & Render -------------------------------------------------------------

scene <- dplyr::bind_rows(
  # sphere(y=5, z=5, x=2, radius=1, material=light(intensity=40)),
  sphere(y=5, z=5, x=-2, radius=1, material=light(intensity=40)),
  group_objects(
    obj_model(
      f, x=-.5, y=-.5, scale=c(1, 1, .25),
      material=diffuse('lightgreen')
    ),
    pivot_point=numeric(3), group_angle=c(xang, 0, 180), 
    group_order_rotation=c(3, 1, 2),
  ),
  cube(y=(-1 * .95)/2, material=water, angle=c(xang - 90, 0, 0)),
  generate_studio(
    material=light(intensity=.975), width=10, height=10,
    distance=-5, curvature=1, depth=-2
  )
)
render_scene(
  scene,
  fov=75,
  width=800, height=800, samples=50,
  # width=400, height=400, samples=5,
  #lookfrom=c(0, .125, .8),
  #lookat=c(0, 0, -.25),
  lookfrom=c(0, .05, .65),
  lookat=c(0, 0, -.25),
  aperture=0,
  clamp_value=5,
  # debug_channel='normals',
  filename=next_file('~/Downloads/derwent/v1/img-000.png')
)
