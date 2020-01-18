# Simple meshes with breaks

source('static/script/mesh-viz/viz-batch-init.R')

a1 <- tris1[[1]][,c(1,3,4,8)]
a2 <- tris2[[1]][,c(2,3)]

b1 <- tris1[[1]][,c(1,4)]
b2 <- tris2[[1]][,2:4]

c1 <- tris1[[1]][,c(1,4)]
c2 <- tris2[[1]][, 4]
c3 <- tris3[[1]][, 2]

seg.a <- dplyr::bind_rows(
  tris_to_seg(a2, map, material=seg.mat2, radius=seg.rad),
  tris_to_seg(a1, map, material=seg.mat1, radius=seg.rad)
)
seg.b <- dplyr::bind_rows(
  tris_to_seg(b2, map, material=seg.mat2, radius=seg.rad),
  tris_to_seg(b1, map, material=seg.mat1, radius=seg.rad)
)
seg.c <- dplyr::bind_rows(
  tris_to_seg(c3, map, material=seg.mat3, radius=seg.rad),
  tris_to_seg(c2, map, material=seg.mat2, radius=seg.rad),
  tris_to_seg(c1, map, material=seg.mat1, radius=seg.rad)
)
xyz1 <- ids_to_xyz(tris1, map, c(1, 1, diff(range(map))))
shard1 <- xyz_to_shard(xyz1, depth=.0125, bevel=45)
obj1 <- shard_to_obj(shard1)
f1 <- tempfile()
writeLines(obj1, f1)

# shards instead of segs

sh.a1 <- tris1[[1]][,c(6,7)]
sh.a2 <- tris1[[1]][,c(5,2)]

sh.b2 <- tris2[[1]][,2]
sh.b3 <- tris2[[1]][,1]
sh.b4 <- tris2[[1]][,4]

sh.c2 <- tris3[[1]][,1]

xyz.a1 <- ids_to_xyz(sh.a1, map, c(1, 1, diff(range(map))))
shr.a1 <- xyz_to_shard(xyz.a1, depth=.0125, bevel=45)
obj.a1 <- shard_to_obj(shr.a1)
f.a1 <- tempfile()
writeLines(obj.a1, f.a1)

xyz.a2 <- ids_to_xyz(sh.a2, map, c(1, 1, diff(range(map))))
shr.a2 <- xyz_to_shard(xyz.a2, depth=.0125, bevel=45)
obj.a2 <- shard_to_obj(shr.a2)
f.a2 <- tempfile()
writeLines(obj.a2, f.a2)

xyz.b2 <- ids_to_xyz(sh.b2, map, c(1, 1, diff(range(map))))
shr.b2 <- xyz_to_shard(xyz.b2, depth=.0125, bevel=45)
obj.b2 <- shard_to_obj(shr.b2)
f.b2 <- tempfile()
writeLines(obj.b2, f.b2)

xyz.b3 <- ids_to_xyz(sh.b3, map, c(1, 1, diff(range(map))))
shr.b3 <- xyz_to_shard(xyz.b3, depth=.0125, bevel=45)
obj.b3 <- shard_to_obj(shr.b3)
f.b3 <- tempfile()
writeLines(obj.b3, f.b3)

xyz.b4 <- ids_to_xyz(sh.b4, map, c(1, 1, diff(range(map))))
shr.b4 <- xyz_to_shard(xyz.b4, depth=.0125, bevel=45)
obj.b4 <- shard_to_obj(shr.b4)
f.b4 <- tempfile()
writeLines(obj.b4, f.b4)

xyz.c2 <- ids_to_xyz(sh.c2, map, c(1, 1, diff(range(map))))
shr.c2 <- xyz_to_shard(xyz.c2, depth=.0125, bevel=45)
obj.c2 <- shard_to_obj(shr.c2)
f.c2 <- tempfile()
writeLines(obj.c2, f.c2)

gang <- c(90, 0, 0)
sobj <- obj_model(filename=f1, material=dielectric(color='#BBBBCC'))
x1 <- x2 <- x3 <- -.5
yoff <- seg.rad/2

# bg1 <- 102

light.narrow <- sphere(
  y=8, z = 2, x = 1, radius = .5,
  material = light(intensity = 200 * mult)
)
bg1 <- 102 / mult
bg1 <- 102
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
scn.base <- dplyr::bind_rows(
  light.narrow,
  # xz_rect(
  #   xwidth=15, zwidth=15, y=10, flipped=TRUE, 
  #   material=diffuse(color='white', lightintensity=1)
  # )
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white'))
)

# scn.a <- dplyr::bind_rows(
#   surf, scn.base,
#   group_objects(
#     seg.a, group_angle=gang, group_translate=c(x1, yoff, zoff),
#     pivot_point=numeric(3)
#   )
# )
# scn.b <- dplyr::bind_rows(
#   surf, scn.base,
#   group_objects(
#     seg.b, group_angle=gang, group_translate=c(x2, yoff, zoff),
#     pivot_point=numeric(3)
#   )
# )
# scn.c <- dplyr::bind_rows(
#   surf, scn.base,
#   group_objects(
#     seg.c, group_angle=gang, group_translate=c(x3, seg.rad/2, zoff),
#     pivot_point=numeric(3)
#   )
# )
scn.a <- dplyr::bind_rows(
  scn.base,
  group_objects(
    dplyr::bind_rows(
      obj_model(filename=f.a1, material=dielectric(color=metal.col[1])),
      obj_model(filename=f.a2, material=dielectric(color=metal.col[1])),
      obj_model(filename=f.b3, material=dielectric(color=metal.col[2])),
      obj_model(filename=f.b4, material=dielectric(color=metal.col[2]))
    ),
    group_angle=gang, group_translate=c(x1, 0, zoff)
  ),
  d1,
  group_objects(d1, group_translate=c(0,1e-3,0)),
  group_objects(d2, group_translate=c(0,.25,0)),
  group_objects(d3, group_translate=c(0,.45,0)),
  # cylinder(y=.5/2, length=.5, z=.5, radius=.05),
)
scn.b <- dplyr::bind_rows(
  scn.base,
  group_objects(
    dplyr::bind_rows(
      obj_model(filename=f.a1, material=dielectric(color=metal.col[1])),
      obj_model(filename=f.b2, material=dielectric(color=metal.col[2])),
      obj_model(filename=f.b3, material=dielectric(color=metal.col[2])),
      obj_model(filename=f.b4, material=dielectric(color=metal.col[2]))
    ),
    group_angle=gang, group_translate=c(x1, 0, zoff)
  ),
  group_objects(d1, group_translate=c(0,1e-3,0)),
  group_objects(d2, group_translate=c(0,.25,0)),
)
scn.c <- dplyr::bind_rows(
  scn.base,
  group_objects(
    dplyr::bind_rows(
      obj_model(filename=f.a1, material=dielectric(color=metal.col[1])),
      obj_model(filename=f.b2, material=dielectric(color=metal.col[2])),
      obj_model(filename=f.c2, material=dielectric(color='#EEAA99')),
    ),
    group_angle=gang, group_translate=c(x1, 0, zoff)
  ),
  group_objects(d1, group_translate=c(0,1e-3,0)),
)
rez <- 600
samp <- 400
scns <- list(scn.a, scn.b, scn.c)
# scns <- list(stest)
file <- next_file('~/Downloads/mesh-viz/small-mesh/simple-mesh-new2-')
render_scenes(
# render_scene(
  list(scn.a, scn.b, scn.c),
  filename='~/Downloads/mesh-viz/small-mesh/composite-mesh-fin-%d.png',
  # scn.a,
  # filename=file,
  height=rez, width=rez, samples=samp,
  lookfrom=c(0, 3, 1.5), lookat=c(0, 0, -.35), fov=27,
  # lookfrom=c(0, .6, 1.5), lookat=c(0, .5, 0), fov=60,
  # lookfrom=c(0, 3, -1.5), lookat=c(0, 0, .5),
  # lookfrom=c(0, 4, 1),
  # lookat=c(0, 0, 0),
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE
)
# Error bars alone

# stop()
# 
# dir <- '~/Downloads/mesh-viz/batch-hirez-4'
# png.in <- sprintf('%s/simple-mesh-s-%d.png', dir, 1:3)
# png.out <- sprintf('%s/simple-mesh-s-merge.png', dir)
# cbind_pngs(png.in, png.out)
# 
