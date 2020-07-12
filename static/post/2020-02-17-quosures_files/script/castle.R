# no idea if these two are the right names
set.seed(12)
nshaft <- 30
phi <- rnorm(nshaft, 0, 40)
theta <- sample(1:360, nshaft)
lens <- pmax(rnorm(nshaft, .75, .3), .1)
shaft.mat <- dielectric(attenuation=c(4, .2, 1))
x <- y <- rep(Inf, nshaft)
while(any(oob <- x^2 + y^2 > .5)) {
  x[oob] <- rnorm(sum(oob), 0, .2)
  y[oob] <- rnorm(sum(oob), 0, .2)
}

# base hex

angles <- seq(0, 2 * pi - pi/3, length.out=6)
hexb <- cbind(sin(angles), cos(angles))
hexscale <- pmax(rnorm(nshaft, .1, .025), .025)

# make kaleidoscope

v1 <- cbind(hexb, 0)[,c(1,3,2)]
v2 <- cbind(hexb[c(2:6, 1),], 0)[,c(1,3,2)]
v3 <- cbind(numeric(6), -1L, numeric(6))

kalei <- lapply(
  seq_len(nrow(v1)),
  function(i) triangle(v1[i,], v2[i,], v3[i,], material=gold_mat)
)
# render_scene(
#   dplyr::bind_rows(
#     kalei,
#     sphere(radius=.2, material=light(intensity=1), y=-.25)
#   ),
#   lookfrom=c(0,5,0.000001),
#   samples=100,
#   fov=30
# )



shafts <- lapply(
  seq_len(nshaft),
  function(i)
    extruded_polygon(
      hexb * hexscale[i], x=x[i], z=z[i],
      top=lens[i], angle=c(phi[i], theta[i], 0), material=shaft.mat,
      material_id=1642L
    )
)
    # generate_ground(
    #   material=diffuse(
    #     checkercolor='grey75', checkerperiod=.5
    #   ), 
    #   spheresize=30,
    #   depth=0
    # ),

render_scene(
  dplyr::bind_rows(
    shafts,
    sphere(x=5, y=5, z=5, radius=2, material=light(intensity=20)),
    sphere(z=-.25, material=light(intensity=2), radius=.25),
    disk(radius=1.25, z=-.75, material=gold_mat),
    disk(radius=1.25, z=-.75, angle=c(-90,0,0), material=diffuse(gold))
  ),
  width=720, height=720, samples=50,
  clamp_value=5,
  # lookfrom=c(0,3,7), lookat=c(0,.75,0),
  lookfrom=c(0,5,5), lookat=c(0,0,0),
  filename=next_file("~/Downloads/rlang/imgs/img-")
)

