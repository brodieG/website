source('static/script/mesh-viz/viz-lib.R')
source('static/script/mesh-viz/rtin-vec.R')
library(vetr)

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
     0.25, 0.5, 0.8,
     0.9, 0.7, 0.9
  ), 3, byrow=TRUE
)[3:1, 3:1]

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
errs2a.cyl <- errs_to_cyl(
  transform(errs2.df, z0=z0 - min(z0), x=x-.5, y=y-.5),
  diffuse(color='grey75', checkercolor='grey35', checkerperiod=.05)
)
errs3a.cyl <- errs_to_cyl(
  transform(errs3.df, z0=z0 - min(z0), x=x-.5, y=y-.5),
  diffuse(color=metal.col[3], checkercolor='grey75', checkerperiod=.05)
)
zoff <- .5
# light.narrow <- sphere( y=8, z = 2, x = 1, radius = .1,
#   material = light(intensity = 5000)
# )
scn.base <- dplyr::bind_rows(
  light.narrow,
  # xz_rect(
  #   xwidth=15, zwidth=15, y=10, flipped=TRUE, 
  #   material=diffuse(color='white', lightintensity=1)
  # )
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white'))
)
mult <- 1.3
light.narrow <- sphere(
  y=8, z = 2, x = 1, radius = .5,
  material = light(intensity = 200 * mult)
)
bg1 <- 102 / mult
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
scn.base <- dplyr::bind_rows(
  light.narrow,
  # xz_rect(
  #   xwidth=15, zwidth=15, y=10, flipped=TRUE,
  #   material=diffuse(color='white', lightintensity=1)
  # )
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white'))
)

