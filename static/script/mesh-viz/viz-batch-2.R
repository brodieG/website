library('rayrender')
library('vetr')
source('viz-lib.R')
source('rtin-vec.R')
source('extract-vec.R')


f <- tempfile()
f2 <- tempfile()
f3 <- tempfile()
f4 <- tempfile()


# Glass full rez mesh

mesh.tri <- mx_to_mesh(vsq)
mesh.tri.s <- scale_mesh(mesh.tri)
mesh.shard <- xyz_to_shard(
  mesh_to_xyz(mesh.tri.s, vsq, rep(1,3)), dept=.0025, bevel=45
)
full.obj <- shard_to_obj(mesh.shard)
writeLines(full.obj, f4)

seg.rad <- .00125 * 1.5
# seg.rad <- .00125 * 2
metal.col <-  c('gold', 'grey75', '#CC3322')
metal.col <- c("#EBC600", "#BFBFBF", "#EF3C28")
metal.col <-  rep('gold', 3)
mesh.colors <- metal.col
seg.mat1 <- metal(color=metal.col[1])
seg.mat2 <- metal(color=metal.col[2])
seg.mat3 <- metal(color=metal.col[3])

seg3 <- tris_to_seg(tris3, map, material=seg.mat3, radius=seg.rad)
seg2 <- tris_to_seg(tris2, map, material=seg.mat2, radius=seg.rad)
seg1 <- tris_to_seg(tris1, map, material=seg.mat1, radius=seg.rad)

zoff <- +.5
xoff <- +.5

obj <- obj_model(f4, material=dielectric(color='#BBBBCC'))

bright.mult <- 1
li <- 2000 * bright.mult

scn.base <- dplyr::bind_rows(
  sphere(
    # y=40, z = 20, x = 10, radius = 1,
    y=40, z = 20, x = 20, radius = 1,
    material = light(intensity = li, importance_sample = TRUE)
  ),
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white'))
  # xz_rect(
  #   y=5, xwidth=15, zwidth=15,
  #   material=diffuse(color='white', lightintensity=2), flipped=TRUE
  # )
)

fblur <- 9
frames <- 240 * 2
angles <- head(seq(from=0, to=360, length.out=frames + 1L), -1L)
# angles <- head(seq(from=0, to=180, length.out=frames + 1L), -1L)
mult <- (cos(angles / 180 * pi) + 1) / 2

# Produce tolerances that lead to regular number of triangle changes

min.tol <- 1.9
max.tol <- 50
tri.n <- vapply(
  seq(min.tol, max.tol, length.out=3000),
  function(x) length(unlist(extract_mesh2(errors, x), 0))/3, 0
)
tol.seq <- 1/seq(1/min.tol, 1/max.tol, length.out=3000)
tri.n2 <- vapply(
  tol.seq,
  function(x) length(unlist(extract_mesh2(errors, x), 0))/3, 0
)
tol.rng <- rev(tapply(tol.seq, tri.n2, range))
tol_fun <- approxfun(
  x=(seq_len(length(tol.rng) + 1) - 1) / length(tol.rng),
  y=c(tol.rng[[1]], vapply(tol.rng[-1], '[', 0, 2))
)
tols <- tol_fun(1 - mult)
# tri.n3 <- vapply(
#   tols[seq(1, length(tols), by=length(tols) / 40)],
#   function(x) length(unlist(extract_mesh2(errors, x), 0))/3, 0
# )
# plot(log2(tri.n3))

# min.tol <- elmax / 25
ramp1 <- colorRamp(rev(metal.col)[1:2], space='Lab')
ramp2 <- colorRamp(rev(metal.col)[2:3], space='Lab')

cols <- character(frames)
if(any(mult < 0.5))
  cols[mult < 0.5] <- rgb(ramp1(mult[mult < 0.5] * 2), maxColorValue=255)
if(any(mult >= 0.5))
  cols[mult >= 0.5] <- rgb(ramp2((mult[mult >= 0.5] - .5) * 2), maxColorValue=255)
rez <- 720
samp <- 300
# rez <- 300
# samp <- 200
gt <- c(-xoff, -0.5, 0)
pp <- c(0.5, 0.5, 0)
bg <- do.call(rgb, c(as.list(c(102,102,102) * bright.mult), max=255))
# bg <- '#606060'
# bg <- '#555555'

dir <- '~/Downloads/mesh/tmp/no-blur-4/'

frame.i1 <- seq(1, frames, by=2)
frame.i2 <- seq(2, frames, by=2)
frame.i <- c(frame.i1, frame.i2)

for(i in frame.i) {
  if(file.exists('stop')) stop("Stop requested and honored at ", Sys.time())
  writeLines(
    sprintf(
      'Frame %d/%d r: %d s: %d l: %d b: %s (%s)', i, frames, rez, samp, li,
      bg, Sys.time()
  ) )
  # ga <-c(90, 90 + angles[i], 0)
  ga <-c(90, angles[i] + 180, 0)
  tris <- extract_mesh2(errors, tols[i])
  seg <- tris_to_seg(tris, map, material=metal(cols[i]), radius=seg.rad)
  joints <-
    tris_to_joints(tris, map, material=metal(cols[i]), radius=seg.rad * 1.6)
  scn <- dplyr::bind_rows(
    scn.base,
    # sphere(radius=.1, material=diffuse(color='green')),
    group_objects(obj, group_angle=ga, group_translate=gt, pivot_point=pp),
    group_objects(seg, group_angle=ga, group_translate=gt, pivot_point=pp),
    group_objects(joints, group_angle=ga, group_translate=gt, pivot_point=pp)
  )
  render_scene(
    scn,
    width=rez, height=rez * 1, samples=samp,
    lookfrom=c(0, 2, 2), lookat=c(0, .28, 0), aperture=0, fov=33.5,
    # lookfrom=c(-.05, 4, 2), lookat=c(-.05, .1, 0), aperture=0, fov=19.5,
    # lookfrom=c(0, 4, 2), lookat=c(.15, .1, 0), aperture=0, fov=5,  #test 0
    clamp=3,
    ambient_light=TRUE,
    backgroundlow=bg,
    backgroundhigh=bg,
    # filename=sprintf('~/Downloads/mesh/tmp/glass-and-mesh-rot-test-%04d.png', i)
    filename=sprintf('%s/glass-and-mesh-%04d.png', dir, i)
    # filename=sprintf('~/Downloads/mesh/tmp/glass-and-mesh-rot-lr-%04d.png', i)
  )
}
stop('done render ', Sys.time())

# We'll need to modify this to copy the last four frames to the beginning

ff <- list.files(dir, pattern='mesh-\\d{4}\\.png', full=TRUE)
fnum <- sub('.*?(\\d{4})\\.png', '\\1', ff)
f.new <- sprintf('%sglm-%04d.png', dir, as.integer(fnum) + 4)
all(file.rename(ff, f.new))

ff2 <- list.files(dir, pattern='glm-\\d{4}\\.png', full=TRUE)
file.copy(tail(ff2, 4), sprintf('%sglm-%04d.png', dir, 1:4))

x <- seq(5, 2160, by=9)
for(i in seq_along(x)) {
  cat(i, ' ')
  # pngs <- lapply(list.files(dir, full.names=TRUE)[x[i] + (-4:4)], png::readPNG)
  pngs <- lapply(list.files(dir, full.names=TRUE)[x[i]], png::readPNG)
  png.all <- Reduce('+', pngs) / length(pngs)
  png::writePNG(
    png.all, sprintf('%s/glm-all-%04d.png', file.path(dir, 'no-blur'), i)
  )
}


scn.1 <- add_object(
  scn.base,
)
scn.2 <- add_object(
  scn.base,
  group_objects(
    seg2, group_angle=c(90, 90, 0), group_translate=c(xoff, 0, zoff),
    pivot_point=numeric(3)
  )
)
scn.3 <- add_object(
  scn.base,
  group_objects(
    seg3, group_angle=c(90, 90, 0), group_translate=c(xoff, 0, zoff),
    pivot_point=numeric(3)
  )
)
# scns <- list(scn.1, scn.2, scn.3)
scns <-  list(scn.1, scn.3)

# render scenes renders multiple scenes
render_scenes(
  scns,
  width=rez, height=rez * 1, samples=samp,
  lookfrom=c(0, 4, 2), lookat=c(-.1, .25, 0), aperture=0, fov=17,
  clamp=3,
  # backgroundimage='~/Downloads/blank.png'
  filename='~/Downloads/mesh/tmp/glass-and-mesh-3-f-%d.png'
)

