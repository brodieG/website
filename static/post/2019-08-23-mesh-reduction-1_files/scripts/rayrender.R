library(rayrender)

set.seed(1220)
x <- runif(10)
x.width <- 1
image <- png::readPNG(sprintf('~/Downloads/ray-anim/img-%04d.png', 1))

raw <- watch(insert_sort, c('i', 'j', 'x'))(x)
dat <- simplify_data(attr(raw, 'watch.data'))
code.txt <- expand_text(attr(raw, 'watch.code'), dat)
dats <- dat[['scalar']]

for(id in unique(dats[['.id']])) {

}

dpi <- 72
width <- 800
height <- width/3*2

floor.mult <- 1.25
wall.mult <- floor.mult/3*2
scene <- xz_rect(
  xwidth=floor.mult*x.width, 
  zwidth=floor.mult*x.width * 1.255555,
  z=-floor.mult*x.width/2,
  y=0,
  # material=lambertian(checkercolor='black', checkerperiod=x.width/length(x) * 3)
  material=lambertian(color='grey50')
)
scene <- add_object(scene,
  yz_rect(
    zwidth=floor.mult*x.width, ywidth=wall.mult*x.width,
    z=-floor.mult*x.width/2, y=wall.mult*x.width/2,
    # material=lambertian(checkercolor='black', checkerperiod=x.width/length(x)),
    material=lambertian(image=image),
    angle=c(0, -90, 0), #flipped=TRUE
) )
# scene <- generate_ground(material = lambertian())

x.squish <- .8
x.width.2 <- x.width * x.squish
x.off <- seq(-x.width.2 / 2, x.width.2/2, length.out=length(x))
for(i in seq_along(x)) {
  scene <- scene %>% add_object(
    cube(
      x=x.off[i], y=x[i] / 2, z=-0.25,
      xwidth=x.width.2 / length(x) * .75,
      zwidth=x.width.2 / length(x) * .75,
      ywidth=x[i],
      material=dielectric(color='#FFFF99'),
      angle=c(0,22.5,0)
    ) 
  )
}
# Add lines

x.delim <- seq(
  -x.width.2 / 2 - x.width.2 / length(x) / 2,
  x.width.2 / 2 + x.width.2 / length(x) / 2,
  length.out=length(x) + 1
)
line.width <- x.width.2 / length(x) * .9 * .1
for(i in seq_along(x.delim)) {
  scene <- add_object(
    scene,
    cube(
      x=x.delim[i], z=-.175, 
      y=line.width/2,
      xwidth=line.width,
      ywidth=line.width,
      zwidth=.25,
      material=lambertian(color='grey50')
    )
  )
}
# Add letters

scene <- add_object(i_factory(z=-0.15, x=x.off[1], scale=rep(0.15,3)), scene)
scene <- add_object(j_factory(z=-0.05, x=x.off[5], scale=rep(0.15,3)), scene)

# Add light source

final <- add_object(
  scene,
  sphere(
    y=3, z = 2, x = 1, radius = .1,
    material = lambertian(lightintensity = 3000, implicit_sample = TRUE)
) )
render_scene(
  final, parallel = TRUE, 
  width = 200, height = 200, samples = 200,
  # width = 600, height = 600, samples = 1000,
  lookfrom=c(0,.75,1), lookat=c(0,.125,-1), fov=45,
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
  dpi=dpi, bg='grey50'
)


## Takes a set of coordinates and makes cubes centered on each of those
## coordinates.  Currently assumes coord are x/y and z == 0. Objects are
## centered on zero in x and scaled such that range(y) == c(0,1)

as_cubes <- function(mx, material=lambertian()) {
  vals <- which(mx, arr.ind=TRUE)
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
    )
  }
}
i_factory <- as_cubes(
  !read.csv('~/Downloads/i.csv', header=F), 
  material=metal(color='green')
)
j_factory <- as_cubes(
  !read.csv('~/Downloads/j.csv', header=F), 
  material=metal(color='green')
)

i.obj <- i_factory(x=-.5)
j.obj <- j_factory(x=.5)

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
