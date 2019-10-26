library(rayrender)

set.seed(1220)
x <- runif(10)
x.width <- 1
x.off <- seq(-x.width / 2, x.width/2, length.out=length(x))

floor.mult <- 1.25
wall.mult <- floor.mult/3*2
scene <- xz_rect(
  xwidth=floor.mult*x.width, 
  zwidth=floor.mult*x.width,
  z=floor.mult*x.width/2,
  y=0,
  # material=lambertian(checkercolor='black', checkerperiod=x.width/length(x) * 3)
  material=lambertian(color='grey35')
)
scene <- add_object(scene,
  yz_rect(
    zwidth=floor.mult*x.width, ywidth=wall.mult*x.width,
    z=floor.mult*x.width/2, y=wall.mult*x.width/2,
    # material=lambertian(checkercolor='black', checkerperiod=x.width/length(x)),
    material=lambertian(image=image),
    angle=c(0, 90, 0), #flipped=TRUE
) )
# scene <- generate_ground(material = lambertian())
for(i in seq_along(x)) {
  scene <- scene %>% add_object(
    cube(
      x=x.off[i], y=x[i] / 2, z=0.25,
      xwidth=x.width / length(x) * .9,
      zwidth=x.width / length(x) * .9,
      ywidth=x[i],
      material=dielectric(color='#FFFF99'),
      angle=c(0,22.5,0)
    )
  )
}

# Add light source

final <- add_object(
  scene,
  sphere(
    y=3, z = -2, x = 1, radius = .1,
    material = lambertian(lightintensity = 3000, implicit_sample = TRUE)
) )
render_scene(
  final, parallel = TRUE, 
  width = 200, height = 200, samples = 200,
  # width = 600, height = 600, samples = 1000,
  lookfrom=c(0,1,-1), lookat=c(0,0,1), fov=45,
  ambient_light=FALSE, aperture=.0, clamp=5
)

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
  dpi=dpi, bg='grey35'
)
image <- png::readPNG(sprintf('~/Downloads/ray-anim/img-%04d.png', i))


## Takes a set of coordinates and makes cubes centered on each of those
## coordinates.  Currently assumes coord are x/y and z == 0. Objects are
## centered on zero in x and scaled such that range(y) == c(0,1)

as_cubes <- function(vals, material=lambertian()) {
  x.rng <- diff(range(vals[,1]))
  y.rng <- diff(range(vals[,1]))
  scale.fac <- 1 / (y.rng + 1)
  vals[,2] <- -vals[,2]
  vals[,1] <- max(vals[,1]) - vals[,1] + 1/2
  vals[,2] <- max(vals[,2]) - vals[,2] - y.rng / 2
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
      group_scale=scale
    )
  }
}
i.vals <- which(!read.csv('~/Downloads/i.csv'), arr.ind=TRUE)
i_factory <- as_cubes(i.vals, material=metal(color='yellow'))
j.vals <- which(!read.csv('~/Downloads/j.csv'), arr.ind=TRUE)
j_factory <- as_cubes(j.vals, material=dielectric(color='green'))

i.obj <- i_factory(x=.5)
j.obj <- j_factory(x=-.5)

floor <- xz_rect(xwidth=3, zwidth=3, material=lambertian(color='grey35'))
light <-  sphere(
  material=lambertian(lightintensity=50, implicit_sample=TRUE),
  x=1, y=1, z=3
)
render_scene(
  add_object(add_object(floor, light), add_object(i.obj, j.obj)),
  width=200, height=200, samples=1000,
  lookfrom=c(0, 2, 2), lookat=c(0, .5, 0), fov=60,
  clamp=5
)
