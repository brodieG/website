source('static/post/2020-02-17-quosures_files/script/portal-prep.R')

# for(j in seq(270, frames.all, by=1)) {
for(j in 1) {
  i <- max(1, j - frames.spin)
  time <- duration * (j - 1) / (frames.all - 1)
  c.angle <- c.angles[i]
  writeLines(sprintf("Frame %04d (%04d) %s", i, j, Sys.time()))
  a <- path.int[, i]
  b <- path.int[, i+1]
  lf <- a + c(0, .5, 0)
  # total hack for dealing for too short steps at beginning and end
  if(sqrt(sum(a - path.int[,ncol(path.int)])^2) < 1e-3) {
    # end
    la <- c.xyz + c(0, .5, 0)
  } else if (all(a == b)) {
    # begining
    la <- c(0, .5, -1)
  } else {
    # normal
    la <- b + c(0, .5, 0)
  }
  lv <- (la - lf)
  lv <- lv / sqrt(sum(lv^2))
  la <- lf + lv
  ld <- sqrt(sum((c.xyz - a)^2)) /
    sqrt(sum((c.xyz - path.int[,ncol(path.int)])^2))
  lu <- .3 / ld

  star.v0 <- (stars.xz - lf) / rep(sqrt(colSums((stars.xz - lf)^2)), each=3)
  star.angle <- acos(colSums(star.v0 * lv)) / pi * 180 *
    sign(xprod(star.v0, lv)[2,])

  # stars that are part of the second set should start at 90 degrees from the
  # angle.  So the angle about the y axis should be start angle + 90 degrees.

  star.rot <- star.meta['angle', ] + star.meta['speed',] * time
  stars <- mapply(
    make_star, stars.all[1,], stars.all[2,], stars.all[3,],
    star.rot,
    flip=(abs(star.rot - star.angle) > 90),
    MoreArgs=list(tc=tc), SIMPLIFY=FALSE
  )
  scene <- dplyr::bind_rows(
    group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
    group_objects(bag, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
    pv.all.obj,
    bubble,
    sphere(z=l.d, x=l.d, radius=l.r, material=light(intensity=l.b)),
    sphere(z=l.d, x=-l.d, radius=l.r, material=light(intensity=l.b)),
    stars,
    group_objects(
      castle_backdrop, group_translate=c.xyz + c.v * c.b.off + c(0, .25, 0),
      group_angle=c(-10, c.angle.0 + 90, 0), pivot_point=numeric(3),
      # group_order_rotation=c(2,1,3)
    ),
    group_objects(
      make_castle(c.size[j]), group_translate=c.xyz + c(0, .30, 0),
      group_angle=c(0, c.angle.0 + c.angle + 90, 0), pivot_point=numeric(3),
      group_order_rotation=c(2,1,3)
    )
  )
  render_scene(
    scene,
    filename=next_file("~/Downloads/rlang/stills/img-"),
    lookfrom=lf, lookat=la+c(0,lu * lookup[i], 0),
    # lookat=c.xyz, lookfrom=c.xyz + c(0, 3, 3),
    # lookfrom=c(0.6, .45, .75), lookat=c.xyz,
    # lookfrom=c(0, 0, 0.1), lookat=c(0, 10, -3.0001),
    # width=720, height=720, samples=200,
    width=1000, height=1000, samples=300,
    # width=200, height=200, samples=3,
    clamp_value=5,
    fov=fov,        # this affects computations above
    aperture=0
  )
}


# objs <- dplyr::bind_rows(
#   make_star(0, 0, 0, 0, flip=FALSE, tc=tc),
#   sphere(material=light(intensity=20), x=5, y=5, z=5, radius=3),
# )
# render_scene(
#   objs, aperture=0, lookfrom=c(1,0,0),
# )

