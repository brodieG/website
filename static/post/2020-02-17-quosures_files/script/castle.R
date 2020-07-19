# no idea if these two are the right names
set.seed(16)
nshaft <- 30
phi <- rnorm(nshaft, 0, 40)
theta <- sample(1:360, nshaft)
lens <- pmax(rnorm(nshaft, .75, .3), .1)
x <- y <- rep(Inf, nshaft)
z <- numeric(nshaft)
while(any(oob <- x^2 + y^2 > .3)) {
  x[oob] <- rnorm(sum(oob), 0, .2)
  y[oob] <- rnorm(sum(oob), 0, .2)
}
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

shaft.mat <- dielectric(attenuation=c(4, .2, 1))
# shaft.mat <- diffuse()
shafts <- lapply(
  seq_len(nshaft),
  function(i)
    extruded_polygon(
      hexb * hexscale[i], x=x[i], z=z[i],
      top=lens[i], angle=c(phi[i], theta[i], 0), material=shaft.mat,
      material_id=1642L
    )
)
backdrop <- dplyr::bind_rows(
  disk(z=-.75, material=gold_mat, radius=1.5),
  disk(z=-.75, material=gold_mat, angle=c(-90, 0, 0), radius=1.5)
)
castle <- dplyr::bind_rows(
  backdrop,
  shafts,
  sphere(radius=.5, material=light(intensity=6), z=-.75),
)
render_scene(
  group_objects(castle, group_angle=c(-10,0,0), pivot_point=c(0,0,-.75)),
  # width=720, height=720, samples=400,
  samples=5,
  clamp_value=5,
  # lookfrom=c(0, 5, 0.0001), lookat=c(0,.25,-.25),
  lookfrom=c(0, 2, 20), lookat=c(0,.25,0),
  filename=next_file("~/Downloads/rlang/castle/img-"),
  aperture=0,
  fov=40
)

