# just the errors

source('static/script/mesh-viz/viz-batch-init.R')

errs2a.cyl <- errs_to_cyl(
  transform(errs2.df, z0=0, x=x-.5, y=y-.5),
  diffuse(color='grey50', checkercolor='grey25', checkerperiod=.05)
)
errs3a.cyl <- errs_to_cyl(
  transform(errs3.df, z0=0, x=x-.5, y=y-.5),
  diffuse(color=metal.col[3], checkercolor='grey75', checkerperiod=.05)
)

# Just the errors

# cmat <- diffuse(color='#CCCCCC')
cmat <- diffuse(color='white')
ang1 <- c(-90, 0, 0)
ang2 <- c(-90, 90, 0)
crad <- .025
grid <- dplyr::bind_rows(
  cylinder(x=-.25, y=0, length=1.5, radius=crad, material=cmat),
  cylinder(x=-.75, y=0, length=1.5, radius=crad, material=cmat),
  cylinder(x=+.25, y=0, length=1.5, radius=crad, material=cmat),
  cylinder(x=+.75, y=0, length=1.5, radius=crad, material=cmat)
)
scn.4 <- dplyr::bind_rows(
  scn.base,
  group_objects(grid, group_angle=c(-90, 0, 0)),
  group_objects(grid, group_angle=c(-90, 90, 0)),
  group_objects(
    errs2a.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  group_objects(
    errs3a.cyl, group_angle=gang,
    pivot_point=numeric(3)
  ),
  # sphere(material=diffuse(color='green'), radius=.1)
  NULL
)
# bg1 <- 102
bg1 <- 80
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
rez <- 200
samp <- 200
# scns <- list(scn.1, scn.2, scn.3)
scns <- list(scn.4)
render_scenes(
  scns, height=rez, width=rez, samples=samp,
  lookfrom=c(0, 4, 1),
  lookat=c(0, 0, 0),
  fov=25,
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE,
  filename='~/Downloads/mesh-viz/small-mesh/simple-mesh-s2-%d.png'
)
# Error bars alone
# 
