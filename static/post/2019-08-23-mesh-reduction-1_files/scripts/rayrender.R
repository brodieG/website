library(rayrender)
library(watcher)

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

floor.mult <- 1.25
wall.mult <- floor.mult/3*2
scn0 <- xz_rect(
  xwidth=floor.mult*x.width, zwidth=floor.mult*x.width * 1.1,
  z=-floor.mult*x.width/2, y=0,
  material=lambertian(color='grey50')
)
scn0 <- add_object(scn0, floor_grooves(x.off))
# Add light source

scn0 <- add_object(
  scn0,
  sphere(
    y=3, z = 2, x = 1, radius = .1,
    material = lambertian(lightintensity = 3000, implicit_sample = TRUE)
) )

for(id in unique(dats[['.id']])) {
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
    file=sprintf('~/Downloads/ray-anim-2/img-%d.png', id),
    scn, parallel = TRUE,
    # width = 200, height = 200, samples = 50,
    width = 600, height = 600, samples = 1000,
    # width = 400, height = 400, samples = 500,
    lookfrom=c(0,.75,1), lookat=c(0,.125,-1), fov=45,
    ambient_light=FALSE, aperture=.0, clamp=5
  )
}

# scene <- generate_ground(material = lambertian())

# Add lines

x.delim <- seq(
  -x.width.2 / 2 - x.width.2 / length(x) / 2,
  x.width.2 / 2 + x.width.2 / length(x) / 2,
  length.out=length(x) + 1
)
line.width <- x.width.2 / length(x) * .9 * .1
# Add letters



library(watcher)
library(gganimate)
library(ggplot2)

# - Text stuff

set.seed(1220)
x <- runif(10)
raw <- watch(insert_sort, c('i', 'j', 'x'))(x)
dat <- simplify_data(attr(raw, 'watch.data'))
code.txt <- expand_text(attr(raw, 'watch.code'), dat)

dpi <- 72
width <- 800
height <- width/3*2
dev.off()
dev.new(width=width/dpi, height=width/dpi, dpi=dpi)

p <- ggplot(subset(code.txt, .id==1)) +
  geom_text(
    aes(x=0, y=y.raw, label=code),
    hjust=0, size=12, family='mono'
  ) + 
  labs(y=NULL, x=NULL) +
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.margin=unit(c(0, 0, height/3/dpi, 0), "inches"),
  ) +
  coord_cartesian(ylim=c(0,12), xlim=c(0, 60)) +
  NULL


i <- 1
ggsave(
  filename=sprintf('~/Downloads/ray-anim/img-%04d.png', i),
  plot=p,
  width=width/dpi, height=height/dpi, units='in', device='png',
  dpi=dpi, bg='grey50'
)



floor <- xz_rect(
  xwidth=10, zwidth=10, 
  material=lambertian(color='grey35')
)
light <-  sphere(
  material=lambertian(lightintensity=50, implicit_sample=TRUE),
  x=1, y=1, z=3
)
render_scene(
  add_object(add_object(floor, light), add_object(i.obj, j.obj)),
  width=200, height=200, samples=200,
  lookfrom=c(0, 1.5, 2), lookat=c(0, .5, 0), fov=60,
  clamp=5
)
