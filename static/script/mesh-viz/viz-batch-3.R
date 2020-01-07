# Simple meshes

source('static/script/mesh-viz/viz-batch-init.R')

seg1 <- tris_to_seg(tris1, map, material=seg.mat1, radius=seg.rad, scale=c(1,1,zscl))
seg2 <- tris_to_seg(tris2, map, material=seg.mat2, radius=seg.rad)
seg3 <- tris_to_seg(tris3, map, material=seg.mat3, radius=seg.rad)

xyz1 <- ids_to_xyz(tris1, map, c(1, 1, zscl))
shard1 <- xyz_to_shard(xyz1, depth=.0125, bevel=45)
obj1 <- shard_to_obj(shard1)
f1 <- tempfile()
writeLines(obj1, f1)

gang <- c(90, 0, 0)
sobj <- obj_model(filename=f1, material=dielectric(color='#BBBBCC'))
x1 <- x2 <- x3 <- -.5
yoff <- seg.rad/2

surf <- group_objects(
  sobj, group_angle=gang, group_translate=c(x1, 0, zoff),
  pivot_point=numeric(3)
)
scn.1 <- dplyr::bind_rows(
  surf, scn.base,
  group_objects(
    seg1, group_angle=gang, group_translate=c(x1, yoff, zoff),
    pivot_point=numeric(3)
  )
)
scn.2 <- dplyr::bind_rows(
  surf, scn.base,
  group_objects(
    seg2, group_angle=gang, group_translate=c(x2, yoff, zoff),
    pivot_point=numeric(3)
  ),
  group_objects(
    errs2.cyl, group_angle=gang, group_translate=c(x2, yoff, zoff),
    pivot_point=numeric(3)
  )
)
scn.3 <- dplyr::bind_rows(
  surf, scn.base,
  group_objects(
    seg3, group_angle=gang, group_translate=c(x3, seg.rad/2, zoff),
    pivot_point=numeric(3)
  ),
  group_objects(
    errs2.cyl, group_angle=gang, group_translate=c(x2, yoff, zoff),
    pivot_point=numeric(3)
  ),
  group_objects(
    errs3.cyl, group_angle=gang, group_translate=c(x3, seg.rad/2, zoff),
    pivot_point=numeric(3)
  )
)


# bg1 <- 102
bg1 <- 120
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
rez <- 300
samp <- 100
# scns <- list(scn.1, scn.2, scn.3)
scns <- list(scn.4)
render_scenes(
  scns, height=rez, width=rez, samples=samp,
  lookfrom=c(0, 4, 1),
  lookat=c(0, 0, -.275),
  fov=21,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE,
  filename='~/Downloads/mesh-viz/small-mesh/simple-mesh-s2-%d.png'
)
# Error bars alone

stop()

dir <- '~/Downloads/mesh-viz/batch-hirez-4'
png.in <- sprintf('%s/simple-mesh-s-%d.png', dir, 1:3)
png.out <- sprintf('%s/simple-mesh-s-merge.png', dir)
cbind_pngs(png.in, png.out)

