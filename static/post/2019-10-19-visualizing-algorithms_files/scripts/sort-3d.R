library(ggplot2)
library(rayrender)
library(watcher)

# ------------------------------------------------------------------------------
# - Helper Functions -----------------------------------------------------------
# ------------------------------------------------------------------------------

# - Letters -

i.dat <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1,
  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), .Dim = c(13L, 7L))
j.dat <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(13L, 7L))

# Generate the letter objects by using cubes as pixels

as_cubes <- function(mx, material=lambertian()) {
  vals <- which(mx != 0, arr.ind=TRUE)
  x.rng <- ncol(mx)
  y.rng <- nrow(mx)
  scale.fac <- 1 / (y.rng + 1)
  vals[,2] <- vals[,2] - 1 - x.rng / 2
  vals[,1] <- y.rng - vals[,1] + 1/2
  vals <- vals * scale.fac

  obj <- NULL
  for(i in 1:nrow(vals)) {
    obj <- add_object(
      obj, cube(
        x=vals[i,2], y=vals[i,1], material=material, width=scale.fac
  ) ) }
  function(
    x=0, y=0, z=0, angle=c(0,0,0), order_rotation=1:3,
    scale=c(1,1,1)
  ) {
    group_objects(
      obj, group_translate=c(x, y, z),
      group_angle=angle, group_order_rotation=order_rotation,
      group_scale=scale, pivot_point=numeric(3)
) } }
i_factory <- as_cubes(i.dat , material=metal(color='#77FF44'))
j_factory <- as_cubes(j.dat, material=metal(color='#77FF44'))

# - Code Display -

backdrop_rect <- function(text, width) {
  dpi <- 72
  widthpx <- 800
  heightpx <- widthpx/3*2

  p <- ggplot(text) +
    geom_text(
      aes(
        x=0, y=y.raw * 1.1, label=code,
        color=I(ifelse(highlight, '#226622', 'black'))
      ),
      hjust=0, size=12, family='mono', fontface='bold'
    ) +
    labs(y=NULL, x=NULL) +
    theme(
      axis.text.x=element_blank(), axis.text.y=element_blank(),
      axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
      panel.grid=element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      plot.margin=unit(c(0, 0, heightpx/3/dpi, 0), "inches"),
    ) +
    coord_cartesian(ylim=c(0,12)*1.1, xlim=c(0, 60)) +
    NULL

  f <- tempfile()
  on.exit(unlink(f))
  ggsave(
    filename=f, plot=p,
    width=widthpx/dpi, height=heightpx/dpi, units='in', device='png',
    dpi=dpi, bg='grey50'
  )
  image <- png::readPNG(f)
  yz_rect(
    zwidth=floor.mult*width, ywidth=wall.mult*width,
    z=-floor.mult*width/2, y=wall.mult*width/2,
    material=lambertian(image=image),
    angle=c(0, -90, 0), #flipped=TRUE
  )
}
# - X "tick marks" -

floor_grooves <- function(xs) {
  x.step <- diff(xs)[1]
  line.width <- x.step / 12
  scn <- NULL

  for(x in xs - x.step / 2) {
    scn <- add_object(
      scn,
      cube(
        x=x, z=-.175,
        y=line.width/2,
        xwidth=line.width,
        ywidth=line.width,
        zwidth=.4,
        material=lambertian(color='grey50')
  ) ) }
  scn
}
# ------------------------------------------------------------------------------
# - Rendering Code -------------------------------------------------------------
# ------------------------------------------------------------------------------

library(rayrender)
library(watcher)

# - Data init -

set.seed(1220)
x <- runif(10)
x.width <- 1
x.width.2 <- x.width * .9

raw <- watch(insert_sort, c('i', 'j', 'x'))(x)
dat <- simplify_data(attr(raw, 'watch.data'))
code.txt <- expand_text(attr(raw, 'watch.code'), dat)
dats <- dat[['.scalar']]

x.off <- seq(-x.width.2 / 2, x.width.2 / 2, length.out=length(x))
x.step <- diff(x.off[length(x.off) - 1:0])
x.off <- c(x.off, x.off[length(x.off)] + x.step)

# - Static Components -

# Floor, grooves, and light source

floor.mult <- 1.25
wall.mult <- floor.mult/3*2
scn0 <- xz_rect(
  xwidth=floor.mult*x.width, zwidth=floor.mult*x.width * 1.1,
  z=-floor.mult*x.width/2, y=0,
  material=lambertian(color='grey50')
)
scn0 <- add_object(scn0, floor_grooves(x.off))
scn0 <- add_object(
  scn0,
  sphere(
    y=3, z = 2, x = 1, radius = .1,
    material = lambertian(lightintensity = 3000, implicit_sample = TRUE)
) )
# - Dynamic Components -

frames <- unique(dats[['.id']])[50]  # one frame only here
for(id in frames) {
  writeLines(sprintf("** Frame %d **", id))
  scn <- scn0
  x <- subset(dat$x, .id == id)[['val']]
  i.val <- dats[['i']][id]
  j.val <- dats[['j']][id]

  # Background with the code

  scn <- add_object(scn, backdrop_rect(subset(code.txt, .id==id), x.width))

  # glass bars

  for(i in seq_along(x)) {
    scn <- add_object(
      scn,
      cube(
        x=x.off[i], y=x[i] / 2, z=-0.25,
        xwidth=x.width.2 / length(x) * .75,
        zwidth=x.width.2 / length(x) * .75,
        ywidth=x[i],
        material=
          if(!is.na(j.val) && j.val == i) dielectric(color='#BBBBFF')
          else dielectric(color='#FFFFBB'),
        angle=c(0,22.5,0)
  ) ) }
  # letters

  lsizes <- rep(0.10, 3)
  if(!is.na(j.val)) {
    scn <- add_object(
      j_factory(
        z=-0.1, x=x.off[j.val], y=lsizes[1]/13/2,
        scale=lsizes, angle=c(90, 0, 0)
      ), scn
    )
  }
  scn <- add_object(
    i_factory(
      z=0.02, x=x.off[i.val], scale=lsizes, y=lsizes[1]/13/2,
      angle=c(90, 0, 0)
    ), scn
  )
  # render

  render_scene(
    # backgroundimage='~/Downloads/transparent.png',
    file=sprintf('~/Downloads/ray-anim-4/img-%d.png', id),
    scn, parallel = TRUE,
    width = 200, height = 200, samples = 50,
    # width = 1200, height = 600, samples = 1000,
    # lookfrom=c(0,.75,1), lookat=c(0,.125,-1), fov=45,    # video view point
    lookfrom=c(.375,.04,.04), lookat=c(0,.125,-1), fov=90,
    ambient_light=FALSE, aperture=.0, clamp=5
  )
}
