# source('static/post/2019-08-23-mesh-reduction-1_files/scripts/viz.R')

# - Test it Out ----------------------------------------------------------------

# Small surface examples

# seg.0 <- mesh_to_seg(mesh.tri.s, map, radius=seg.rad, material=diffuse(color='red'))

library(rayrender)

seg.rad <- .0025
seg.rad <- .01
seg.mat1 <- metal(color=metal.col[1])
seg.mat2 <- metal(color=metal.col[2])
seg.mat3 <- metal(color=metal.col[3])

zoff <- +.5

map <- matrix(
  c(
     1.0, 0.9, 0.9,
     0.0, 0.5, 0.8,
     0.9, 0.7, 0.9
  ), 3, byrow=TRUE
)
tris1 <- list(
  matrix(
    c(
      1,5,2, 1,4,5, 3,6,5, 3,2,5,
      7,4,5, 7,8,5, 9,8,5, 9,5,6
    ),
    nrow=3
) )
tris2 <- list(matrix(c(1,3,5, 1,5,7, 7,5,9, 9,5,3), 3))
tris3 <- list(matrix(c(1,3,9, 1,7,9), 3))
zscl <- 1

seg1 <- tris_to_seg(tris1, map, material=seg.mat1, radius=seg.rad, scale=c(1,1,zscl))
seg2 <- tris_to_seg(tris2, map, material=seg.mat2, radius=seg.rad)
seg3 <- tris_to_seg(tris3, map, material=seg.mat3, radius=seg.rad)

xyz1 <- tris_to_xyz(tris1, map, c(1, 1, zscl))
shard1 <- xyz_to_shard(xyz1, depth=.025, bevel=45)
obj1 <- shard_to_obj(shard1)
writeLines(obj1, f1)

# Need to recompute middle error b/c of auto-carryover

errs2 <- errs3 <- compute_error(map)
errs2[5] <- 0
errs3[5] <- (map[1] + map[9]) / 2 - map[5]
errs3[c(2,4,6,8)] <- 0

errs2.df <- mx_to_df(errs2, scale=c(1, 1, 0))
errs2.df[['z0']] <- mx_to_df(map, scale=c(1, 1, 0))[['z']]
errs3.df <- mx_to_df(errs3, scale=c(1, 1, 0))
errs3.df[['z0']] <- mx_to_df(map, scale=c(1, 1, 0))[['z']]
errs_to_cyl <- function(errs.df, mat) {
  errs.df <- subset(errs.df, z > 0)
  dplyr::bind_rows(
    lapply(
      seq_len(nrow(errs.df)),
      function(i) {
        rad <- .05
        ang <- c(-90,0,0)
        with(
          errs.df[i,],
          add_object(
            cylinder(
              x, y, z0 + z/2, radius=rad, angle=ang, length=z,
              material=mat
            ),
            disk(x, y, z0 + z, radius=rad, mat=mat, angle=ang)
        ) )
      }
  ) )
}
errs2.cyl <- errs_to_cyl(
  errs2.df,
  diffuse(color=metal.col[2], checkercolor='grey25')
)
errs3.cyl <- errs_to_cyl(
  errs3.df,
  diffuse(color=metal.col[3], checkercolor='grey75')
)

zoff <- .5
light.narrow <- sphere(
  y=8, z = 6, x = 0, radius = .2,
  material = diffuse(lightintensity = 3000, implicit_sample = TRUE)
)
light.old <- sphere(
  y=2, z = 3, x = 0, radius = .2,
  material = diffuse(lightintensity = 500, implicit_sample = TRUE)
)
gang <- c(90, 0, 0)
sobj <- obj_model(filename=f1, material=dielectric('#CCCCDD'))
x1 <- x2 <- x3 <- -.5
yoff <- seg.rad/2

scn.base <- dplyr::bind_rows(
  light.narrow,
  group_objects(
    sobj, group_angle=gang, group_translate=c(x1, 0, zoff),
    pivot_point=numeric(3)
  ),
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white')),
  xz_rect(
    xwidth=15, zwidth=15, y=10, flipped=TRUE, 
    material=diffuse(color='white', lightintensity=2)
  )
)
scn.1 <- dplyr::bind_rows(
  scn.base,
  group_objects(
    seg1, group_angle=gang, group_translate=c(x1, yoff, zoff),
    pivot_point=numeric(3)
  )
)
scn.2 <- dplyr::bind_rows(
  scn.base,
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
  scn.base,
  group_objects(
    seg3, group_angle=gang, group_translate=c(x3, seg.rad/2, zoff),
    pivot_point=numeric(3)
  ),
  group_objects(
    errs3.cyl, group_angle=gang, group_translate=c(x3, seg.rad/2, zoff),
    pivot_point=numeric(3)
  )
)

rez <- 200
samp <- rez / 2
scns <- list(scn.1)
render_scenes(
  scns, height=rez/400*150, width=rez, samples=samp,
  lookfrom=c(0, 4, .5),
  lookat=c(0, 0, -.125),
  fov=25,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  filename='~/Downloads/mesh-viz/simple-mesh-s-%d.png'
  # backgroundimage='~/Downloads/blank.png'
)
