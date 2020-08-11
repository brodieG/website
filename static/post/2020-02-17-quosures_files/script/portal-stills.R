# source('static/post/2020-02-17-quosures_files/script/portal-prep.R')

# - Long Road ------------------------------------------------------------------

# lv2 <- c.xyz - path.int[, ncol(path.int)]
# lv2 <- lv2 / sqrt(sum(lv2^2))
# lf <-  c.xyz + lv2 * c(-1, 1, 1) * 5 + c(2,1.5,0)
# # lf <- path.int[, 150] + c(0,.5,0)
# la <- c.xyz + c(0,.8,0)
# lv <- la - lf
# lv <- lv / sqrt(sum(lv^2))
# 
# 
# c.angle <- 0
# star.v0 <- (stars.xz - lf) / rep(sqrt(colSums((stars.xz - lf)^2)), each=3)
# star.angle <- acos(colSums(star.v0 * lv)) / pi * 180 *
#   sign(xprod(star.v0, lv)[2,]) * -1
# stars <- mapply(
#   make_star, stars.all[1,], stars.all[2,], stars.all[3,],
#   star.angle,
#   flip=FALSE,
#   MoreArgs=list(tc=tc), SIMPLIFY=FALSE
# )
# 
# bag <- comp_inside(h2, h2.b, bag.mat, light_index=-1, intensity=5)
# scene <- dplyr::bind_rows(
#   group_objects(bag, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
#   group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
#   pv.all.obj,
#   bubble,
#   sphere(z=l.d, x=l.d, radius=l.r, material=light(intensity=l.b)),
#   sphere(z=l.d, x=-l.d, radius=l.r, material=light(intensity=l.b)),
#   stars,
#   group_objects(
#     castle_backdrop, group_translate=c.xyz + c.v * c.b.off + c(0, .25, 0),
#     group_angle=c(-10, c.angle.0 + 90, 0), pivot_point=numeric(3),
#     # group_order_rotation=c(2,1,3)
#   ),
#   group_objects(
#     make_castle(c.size[j]), group_translate=c.xyz + c(0, .30, 0),
#     group_angle=c(0, c.angle.0 + c.angle + 90, 0), pivot_point=numeric(3),
#     group_order_rotation=c(2,1,3)
#   )
# )
# render_scene(
#   scene,
#   filename=next_file("~/Downloads/rlang/stills/img-"),
#   # lookfrom=lf, lookat=la,
#   # lookfrom=c(0, 5, -30), lookat=c(-60, 0.5, 0),
#   lookat=c(-45,0,0), lookfrom=lf,
#   # lookfrom=c(0.6, .45, .75), lookat=c.xyz,
#   # lookfrom=c(0, 0, 0.1), lookat=c(0, 10, -3.0001),
#   # width=720, height=720, samples=1,
#   # width=1000, height=1000, samples=300,
#   width=1600, height=800, samples=200,
#   clamp_value=5,
#   fov=60,
#   aperture=0
# )

# - Inside Out -----------------------------------------------------------------

# lf <- c(0, 2, -20)
# la <- c(0, .5, 0)
# 
# lv <- la - lf
# lv <- lv / sqrt(sum(lv^2))
# 
# c.angle <- 0
# star.v0 <- (stars.xz - lf) / rep(sqrt(colSums((stars.xz - lf)^2)), each=3)
# star.angle <- acos(colSums(star.v0 * lv)) / pi * 180 *
#   sign(xprod(star.v0, lv)[2,]) * -1
# stars <- mapply(
#   make_star, stars.all[1,], stars.all[2,], stars.all[3,],
#   star.angle,
#   flip=TRUE,
#   MoreArgs=list(tc=tc), SIMPLIFY=FALSE
# )
# 
# bag <- comp_inside(h2, h2.b, bag.mat, light_index=-1, intensity=5)
# scene <- dplyr::bind_rows(
#   group_objects(bag, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
#   group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
#   pv.all.obj,
#   bubble,
#   sphere(z=l.d, x=l.d, radius=l.r, material=light(intensity=l.b)),
#   sphere(z=l.d, x=-l.d, radius=l.r, material=light(intensity=l.b)),
#   stars,
#   group_objects(
#     castle_backdrop, group_translate=c.xyz + c.v * c.b.off + c(0, .25, 0),
#     group_angle=c(-10, c.angle.0 + 90, 0), pivot_point=numeric(3),
#     # group_order_rotation=c(2,1,3)
#   ),
#   group_objects(
#     make_castle(c.size[j]), group_translate=c.xyz + c(0, .30, 0),
#     group_angle=c(0, c.angle.0 + c.angle + 90, 0), pivot_point=numeric(3),
#     group_order_rotation=c(2,1,3)
#   )
# )
# render_scene(
#   scene,
#   filename=next_file("~/Downloads/rlang/stills/img-"),
#   # lookfrom=lf, lookat=la,
#   # lookfrom=c(0, 5, -30), lookat=c(-60, 0.5, 0),
#   lookat=la, lookfrom=lf,
#   # lookfrom=c(0.6, .45, .75), lookat=c.xyz,
#   # lookfrom=c(0, 0, 0.1), lookat=c(0, 10, -3.0001),
#   # width=720, height=720, samples=1,
#   # width=1000, height=1000, samples=300,
#   # width=1600, height=800, samples=200,
#   samples=100,
#   clamp_value=5,
#   fov=3,
#   aperture=0
# )

# - Side-by-Side ---------------------------------------------------------------

url <- 'https://www.r-project.org/logo/Rlogo.svg'
zz <- parse_svg(url)
ww <- lapply(
  interp_paths(zz, normalize=TRUE),
  function(x) {
    x[['coords']][] <- lapply(x[['coords']], '-', .5)
    x[['coords']]
  }
)
blue <- "#012060"
# gray <- "grey50"
gray <- "grey20"

hula <- extrude_path(ww[[1]], material=diffuse(gray), top=.05, bottom=-.05)
R <- extrude_path(ww[[2]], material=diffuse(blue), top=.1, bottom=-.1)

lf <-  c(-.5, .5, 1)
la <- c(-.5, .5, 0)
# lf <- path.int[, 150] + c(0,.5,0)
la <- c.xyz + c(0,.8,0)
lv <- la - lf
lv <- lv / sqrt(sum(lv^2))

star.v0 <- (stars.xz - lf) / rep(sqrt(colSums((stars.xz - lf)^2)), each=3)
star.angle <- acos(colSums(star.v0 * lv)) / pi * 180 *
  sign(xprod(star.v0, lv)[2,]) * -1
stars <- mapply(
  make_star, stars.all[1,], stars.all[2,], stars.all[3,],
  star.angle,
  flip=FALSE,
  MoreArgs=list(tc=tc), SIMPLIFY=FALSE
)

scene <- dplyr::bind_rows(
  group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
  pv.all.obj,
  bubble,
  sphere(z=l.d, x=l.d, radius=l.r, material=light(intensity=l.b)),
  sphere(z=l.d, x=-l.d, radius=l.r, material=light(intensity=l.b)),
  stars,
  group_objects(
    dplyr::bind_rows(hula, R), group_translate=c(-1, .5, 0),
    group_angle=c(-90, 0, 0), pivot_point=numeric(3),
    # group_order_rotation=c(2,1,3)
  )
)
render_scene(
  scene,
  filename=next_file("~/Downloads/rlang/stills/img-"),
  lookfrom=lf, lookat=-la,
  # lookat=c.xyz, lookfrom=c.xyz + c(0, 3, 3),
  # lookfrom=c(0.6, .45, .75), lookat=c.xyz,
  # lookfrom=c(0, 0, 0.1), lookat=c(0, 10, -3.0001),
  # width=720, height=720, samples=1,
  # width=1000, height=1000, samples=300,
  width=200, height=200, samples=3,
  clamp_value=5,
  fov=60,
  aperture=0
)

