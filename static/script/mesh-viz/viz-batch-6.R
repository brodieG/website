# Testing errors only for all of volcano

source('static/script/mesh-viz/viz-batch-init.R')

vsq <- matrix(0, 65, 65)
vsq[1:65, 3:63] <- volcano[1:65,1:61]
vsq[1:65, 1:2] <- volcano[1:65, 1]
vsq[1:65, 64:65] <- volcano[1:65, 61]

approx <- rtini::rtini_error(vsq, carry.child=FALSE, approx.instead=TRUE)
err <- rtini::rtini_error(vsq, carry.child=FALSE, approx.instead=FALSE)
ztop <- pmax(approx, vsq)
zbot <- pmin(approx, vsq)

errs.df <- mx_to_df(ztop, scale=numeric(3))
map.df <- mx_to_df(zbot, scale=numeric(3))
errs.df[['z0']] <- map.df[['z']]
errs.df[] <- lapply(errs.df, '-', 1)

# Determine what level of approx things are coming from and color appropriately

errs.df[['lvl']] <- 0
for(i in 6:0) {
  this.lvl <- with(errs.df, (!x %% 2^i) & (!y %% 2^i))
  errs.df[this.lvl & errs.df$lvl == 0, 'lvl'] <- i
}
ramp1 <- colorRamp(rev(metal.col)[3:2], space='Lab')
ramp2 <- colorRamp(rev(metal.col)[2:1], space='Lab')
colors <- c(
  rgb(ramp1(0:3 / 3), maxColorValue=255),
  rgb(ramp2(0:3 / 3), maxColorValue=255)[-1]
)
errs.df[1:2] <- errs.df[1:2] / 64
errs.float <- errs.ground <- errs.df
errs.ground <- transform(errs.df, z=(z-z0) / diff(range(z - z0)))
errs.float[3:4] <-
  (errs.float[3:4] - min(errs.float[3:4])) / diff(range(unlist(errs.float[3:4])))

rad <- .006
# errs.cyl.float <- dplyr::bind_rows(
#   lapply(
#     seq_along(colors),
#     function(i) {
#       mat <- diffuse(color=colors[i], checkercolor='grey75', checkerperiod=rad)
#       errs.cyl <- errs_to_cyl(
#         transform(subset(errs.float, lvl == i - 1), x=x-.5, y=y-.5), mat, rad
#       )
#     }
# ) )
errs.cyl.ground <- dplyr::bind_rows(
  lapply(
    seq_along(colors),
    function(i) {
      mat <- diffuse(color=colors[i], checkercolor='grey75', checkerperiod=rad)
      errs.cyl <- errs_to_cyl(
        transform(
          subset(errs.ground, lvl == i - 1), x=x-.5, y=y-.5, z0=0
        ), mat, rad
      )
    }
) )
dots <- base_points(
  which(err == 0, arr.ind=TRUE), n=nrow(err), mat=dot.mat, rad=rad/3
)

light.narrower <- sphere(
  y=8, z = 2, x = 1, radius = .1,
  material = light(intensity = 200 * mult * 25)
)
bg1 <- 102 / mult
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
scn.base2 <- dplyr::bind_rows(
  light.narrower,
  xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white'))
)

gang <- c(90, 0, 0)
# errs.cyl <- errs.cyl.float
errs.cyl <- errs.cyl.ground
scn <- dplyr::bind_rows(
  scn.base2,
  dots,
  group_objects(
    errs.cyl, group_angle=gang,
    pivot_point=numeric(3),
  ),
  NULL
)

# rez <- 600
# samp <- 100
rez <- 800
samp <- 200

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
  # lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=28,
  lookfrom=c(0, 3, 1.5), lookat=c(0, .25, 0), fov=23,
  # lookat=c(0, .25, 0), fov=9,
  # lookat=c(-.25, .5, 0), fov=15,
  # lookfrom=c(2, 1, 2), lookat=c(0, 0, 0),
  # fov=10,
  # fov=0, ortho_dimensions=c(1.75,1.75),
  aperture=0,
  camera_up=c(0,1,0),
  clamp=3,
  backgroundlow=bg, backgroundhigh=bg,
  ambient_light=TRUE
)
