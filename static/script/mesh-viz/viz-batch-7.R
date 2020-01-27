# final viz

source('static/script/mesh-viz/viz-batch-init.R')

map <- vsq
errors <- rtini::rtini_error(map)
tol <- elmax * .05

mesh <- rtini::rtini_extract(errors, tol)
xyz <- ids_to_xyz(mesh, map, rep(1, 3))

# For each triangle compute the size by measuring distance from first vertex to
# barycenter in x/y plane

xy <- lapply(xyz[1:2], matrix, 3)
xy.b <- lapply(xy, colMeans)
xy.1 <- lapply(xy, '[', 1,)
sizes <- round(sqrt(Reduce('+', Map(function(a, b) (b - a) ^ 2, xy.b, xy.1))), 5)
size.all <- match(sizes, sort(unique(sizes)))

xyzm <- lapply(xyz, matrix, 3)

col1 <- metal.col[1]
col2 <- metal.col[2]
col3 <- '#EEAA99'

ramp1 <- colorRamp(c(col1, col2), space='Lab')
ramp2 <- colorRamp(c(col2, col3), space='Lab')

cols <- c(
  rgb(ramp1((0:3) / 3), maxColorValue=255),
  rgb(ramp2((1:3) / 3), maxColorValue=255)
)

objsf <- lapply(
  1:7,
  function(i) {
    xyz2 <- lapply(xyzm, '[', , size.all == i)
    shards <- xyz_to_shard(xyz2, depth=.0025, bevel=45)
    f <- tempfile()
    full.obj <- shard_to_obj(shards)
    writeLines(full.obj, f)
    f
  }
)
objs <- dplyr::bind_rows(
  lapply(
    seq_along(objsf),
    function(i) {
      obj_model(objsf[[i]], material=dielectric(color=cols[i]))
} ) )
zoff <- +.5
xoff <- +.5

bright.mult <- 1
li <- 2000 * bright.mult
gt <- c(-xoff, -0.5, 0)
pp <- c(0.5, 0.5, 0)
ga <-c(90, 90, 0)
bg <- do.call(rgb, c(as.list(c(102,102,102) * bright.mult), max=255))

scn.base <- dplyr::bind_rows(
  sphere(
    y=40, z = 20, x = 20, radius = 1,
    material = light(intensity = li, importance_sample = TRUE)
  ),
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white')),
)

scene <- dplyr::bind_rows(
  scn.base,
  group_objects(objs, group_angle=ga, group_translate=gt, pivot_point=pp)
)
rez <- 1000
samp <- 500
file <- next_file('~/Downloads/mesh-viz/harlequin/harl-')
render_scene(
  scene, width=rez, height=rez, samples=samp,
  # lookfrom=c(0, 2, 2), lookat=c(0, .28, 0), aperture=0, fov=33.5,
  lookfrom=c(-.12, 4, 1.925), lookat=c(-.12, .1, -.075), aperture=0, fov=17,
  # ortho_dimensions=c(.2,.2),
  # camera_up=c(1,0,0),
  # ortho_dimensions=c(1.05,1.05),
  clamp=3, ambient=TRUE, backgroundlow=bg, backgroundhigh=bg, 
  file=file
)
