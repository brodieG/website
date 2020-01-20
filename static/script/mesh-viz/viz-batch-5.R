source('static/script/mesh-viz/viz-batch-init.R')

gang <- c(90, 0, 0)
disks <- dplyr::bind_rows(
  disk(radius=dro, inner_radius=dri, y=.7, material=mat.d),
#   disk(radius=dro, inner_radius=dri, y=.2, material=mat.d),
#   disk(radius=dro, inner_radius=dri, y=.1, material=mat.d),
#   disk(radius=dro, inner_radius=dri, y=.05, material=mat.d),
)
arr.mat <- metal(color='grey50')
xw <- .3
arr.base <- dplyr::bind_rows(
  xz_rect(zwidth=.025, xwidth=xw, x=.15 + xw/2, material=arr.mat),
  triangle(v1=c(.1, 0, 0), v2=c(.2, 0, .1), v3=c(.2, 0, -.1), material=arr.mat)
)
arrows <- dplyr::bind_rows(
  group_objects(arr.base, group_translate=c(0, .7, 0)),
  # group_objects(
  #   arr.base, group_translate=c(0, .05, 0), group_angle=c(0, 90, 0),
  #   pivot_point=numeric(3)
  # ),
  # group_objects(
  #   arr.base, group_translate=c(0, .1, 0), group_angle=c(0, 180, 0),
  #   pivot_point=numeric(3)
  # ),
  # group_objects(
  #   arr.base, group_translate=c(0, .2, 0), group_angle=c(0, 270, 0),
  #   pivot_point=numeric(3)
  # ),
)
rad <- .05
dots <- base_points(
  which(errs1 == 0, arr.ind=TRUE), n=nrow(errs1), mat=dot.mat, rad=rad/3
)
scn <- dplyr::bind_rows(
  scn.base2,
  disks, arrows,
  dots,
  group_objects(
    errs2b.cyl, group_angle=gang,
    pivot_point=numeric(3),
  ),
  group_objects(
    errs3b.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  NULL
)

# rez <- 800
# samp <- 200
rez <- 700
samp <- 300

# scns <- list(scn.4a, scn.4b, scn.4c)
# scns <- list(scn.4b)
file <- next_file('~/Downloads/mesh-viz/small-mesh/mesh-err-')
# file <- '~/Downloads/mesh-viz/small-mesh/mesh-err-001.png'
render_scene(
  scn,
  filename=file,
# render_scenes(
#   list(scn.4a, scn.4b, scn.4c),
#   filename='~/Downloads/mesh-viz/small-mesh/water-new7g-%d.png',
  height=rez, width=rez, samples=samp,
  # lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=25,
  lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=30,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE
)

scn <- dplyr::bind_rows(
  scn.base2,
  disks, arrows,
  dots,
  group_objects(
    errs2b.cyl, group_angle=gang,
    pivot_point=numeric(3),
  ),
  group_objects(
    errs3b.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  cylinder(y=.45 + .25/2, length=.25, radius=0.05, material=silv.mat),
  disk(y=.7, radius=0.05, material=silv.mat),
  NULL
)
file <- next_file('~/Downloads/mesh-viz/small-mesh/mesh-err-')
# file <- '~/Downloads/mesh-viz/small-mesh/mesh-err-001.png'
render_scene(
  scn,
  filename=file,
# render_scenes(
#   list(scn.4a, scn.4b, scn.4c),
#   filename='~/Downloads/mesh-viz/small-mesh/water-new7g-%d.png',
  height=rez, width=rez, samples=samp,
  lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=30,
  # lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=25,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE
)
