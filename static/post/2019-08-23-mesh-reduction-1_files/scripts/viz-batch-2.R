# source('static/post/2019-08-23-mesh-reduction-1_files/scripts/viz.R')

library('rayrender')

f <- tempfile()
f2 <- tempfile()
f3 <- tempfile()
f4 <- tempfile()

vsq <- matrix(0, 65, 65)
vsq[1:65, 3:63] <- volcano[1:65,1:61]
vsq[1:65, 1:2] <- volcano[1:65, 1]
vsq[1:65, 64:65] <- volcano[1:65, 61]

map <- vsq
errors <- compute_error(map)
elmax <- diff(range(map))
tris1 <- extract_mesh2(errors, elmax/25)
tris2 <- extract_mesh2(errors, elmax/10)
tris3 <- extract_mesh2(errors, elmax/3)

# Glass full rez mesh

mesh.tri <- mx_to_mesh(vsq)
mesh.tri.s <- scale_mesh(mesh.tri)
mesh.shard <- xyz_to_shard(
  mesh_to_xyz(mesh.tri.s, vsq, rep(1,3)), dept=.0025, bevel=45
)
full.obj <- shard_to_obj(mesh.shard)
writeLines(full.obj, f4)

seg.rad <- .00125 * 3
seg.mat <- metal(color='gold')
metal.col <-  c(gold, 'grey75', '#CC3322')
mesh.colors <- metal.col
seg.mat1 <- metal(color=metal.col[1])
seg.mat2 <- metal(color=metal.col[2])
seg.mat3 <- metal(color=metal.col[3])

seg3 <- tris_to_seg(tris3, map, material=seg.mat3, radius=seg.rad)
seg2 <- tris_to_seg(tris2, map, material=seg.mat2, radius=seg.rad)
seg1 <- tris_to_seg(tris1, map, material=seg.mat1, radius=seg.rad)

zoff <- +.5
xoff <- +.5

scn.base <- dplyr::bind_rows(
  sphere(
    y=4, z = 4, x = 0, radius = .2,
    material = diffuse(lightintensity = 1000, implicit_sample = TRUE)
  ),
  group_objects(
    obj_model(f4, material=dielectric(color='#CCCCDD')),
    # obj_model(f4, material=diffuse(color='#CCCCCC')),
    group_angle=c(90, 90, 0), group_translate=c(xoff, 0, zoff),
    pivot_point=numeric(3)
  ),
  xz_rect(xwidth=5, zwidth=5, material=diffuse(color='white')),
  xz_rect(y=5, xwidth=15, zwidth=15, material=diffuse(color='white'), flipped=TRUE)
)
rez <- 200
samp <- rez
scn.1 <- add_object(
  scn.base,
  group_objects(
    seg1, group_angle=c(90, 90, 0), group_translate=c(xoff, 0, zoff),
    pivot_point=numeric(3)
  )
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
scns <- list(scn.1)
lapply(
  seq_along(scns),
  function(i) {
    writeLines(sprintf("Starting frame %d at %s", i, as.character(Sys.time())))
    render_scene(
      scns[[i]],
      width=rez, height=rez, samples=samp,
      lookfrom=c(0, 4, 2), lookat=c(0, .25, 0), aperture=0, fov=17,
      clamp=3,
      file=sprintf('~/Downloads/mesh-viz/glass-and-mesh-3-f-%d.png', i)
    )
  }
)
stop()

a <- png::readPNG(sprintf('~/Downloads/mesh-viz/glass-and-mesh-3-f-%d.png', 1))
b <- png::readPNG(sprintf('~/Downloads/mesh-viz/glass-and-mesh-3-f-%d.png', 2))
c <- png::readPNG(sprintf('~/Downloads/mesh-viz/glass-and-mesh-3-f-%d.png', 3))

d <- array(numeric(), c(800,800*3,3))
d[1:800,1:800,1:3] <- a
d[1:800,801:1600,1:3] <- b
d[1:800,1601:2400,1:3] <- c

png::writePNG(d, sprintf('~/Downloads/mesh-viz/glass-and-mesh-3-f-merge.png'))
