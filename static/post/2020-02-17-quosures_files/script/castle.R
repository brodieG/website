# no idea if these two are the right names
set.seed(16)
# set.seed(18)
nshaft <- 30
phi <- rnorm(nshaft, 0, 40)
theta <- sample(1:360, nshaft)
lens <- pmax(rnorm(nshaft, .75, .3), .1)

n <- nshaft
x <- y <- z <- numeric(n)
xseed <- (runif(n * 20) - .5)
yseed <- (runif(n * 20) - .5)
j <- 1
rad <- .3
dmin <- rad / 7
dmin2 <- dmin^2

for(i in seq_len(n)) {
  while(
    any(
      (x - xseed[j]) ^ 2 + (y - yseed[j]) ^ 2 < dmin2 |
      (xseed[j] ^ 2 + yseed[j] ^ 2 > rad ^ 2)
  ) )
    j <- j + 1
  x[i] <- xseed[j]
  y[i] <- yseed[j]
  j <- j + 1
}
plot(cbind(x, y))
xx <- seq(0, 2 * pi, length.out=20)
m <- .3
points(cos(xx) * m, sin(xx) * m, col='red')
# base hex

angles <- seq(0, 2 * pi - pi/3, length.out=6)
hexb <- cbind(sin(angles), cos(angles))
hexscale <- pmax(rnorm(nshaft, .1, .025), .025)

# # make kaleidoscope
# 
# hexc <- hexb * 1.50
# v1 <- cbind(hexc, 0)[,c(1,3,2)]
# v2 <- cbind(hexc[c(2:6, 1),], 0)[,c(1,3,2)]
# v3 <- cbind(numeric(6), -1L, numeric(6))
# 
# kalei <- lapply(
#   seq_len(nrow(v1)),
#   function(i) 
#   triangle(
#     v1[i,], v2[i,], v3[i,], 
#     material=diffuse(gold)
#     # material=gold_mat
#   )
# )

# shaft.mat <- dielectric(attenuation=c(4, .2, 1))
shaft.mat <- dielectric(attenuation=c(10, 2, 4))
# shaft.mat <- diffuse()

shafts <- lapply(
  seq_len(nshaft),
  function(i)
    extruded_polygon(
      hexb * hexscale[i], x=x[i], z=z[i],
      top=lens[i], angle=c(phi[i], theta[i], 0), material=shaft.mat,
      material_id=1642L,
      scale=rep(2, 3)
    )
)
castle_backdrop <- dplyr::bind_rows(
  disk(material=gold_mat, radius=3.0),
  disk(material=gold_mat, angle=c(90, 0, 0), radius=3.0)
  # disk(
  #   material=diffuse(gold, checkerperiod=.25, checkercolor='white'), radius=1.75
  # ),
  # disk(
  #   material=diffuse('green', checkerperiod=.25, checkercolor='white'), 
  #   angle=c(-90, 0, 0), radius=1.75
  # )
)
castle.size <- .5
make_castle <- (function(shafts) {
  force(shafts)
  function() {
    castle <- dplyr::bind_rows(
      shafts,
      sphere(
        radius=castle.size, material=light(intensity=1), z=0, y=castle.size/2
      ),
    )
  }
})(shafts)

# for(i in seq(0, 360 / angles * (angles - 1), length.out=angles)) {
#   print(sprintf("Frame %d %s", i, Sys.time()))
#   render_scene(
#     dplyr::bind_rows(
#       sphere(x=2, y=2, z=2, material=light(intensity=2)),
#       group_objects(
#         backdrop, group_angle=c(-10,0,0), group_translate=c(0,0,-1.25),
#         pivot_point=c(0,0,0)
#     ) ),
#     # width=720, height=720, samples=100,
#     samples=2,
#     clamp_value=5,
#     # lookfrom=c(0, 5, 0.0001), lookat=c(0,.25,-.25),
#     lookfrom=c(0, .5, 4), lookat=c(0,.40,0),
#     filename=next_file("~/Downloads/rlang/castle/img-"),
#     aperture=0,
#     fov=40
#   )
# }
# print(Sys.time())
