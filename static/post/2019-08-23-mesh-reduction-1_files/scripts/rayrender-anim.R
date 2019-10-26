library(ggplot2)
# Generate the watch data

# Generate the letter objects

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
  material=metal(color='#77FF44')
  # material=dielectric(color='green')
  # material=lambertian(color='black')
)
j_factory <- as_cubes(
  !read.csv('~/Downloads/j.csv', header=F), 
  material=metal(color='#77FF44')
  # material=dielectric(color='green')
  # material=lambertian(color='black')
)
# Function to generate background image

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

  ggsave(
    filename=sprintf('~/Downloads/ray-anim/img-%04d.png', 1),
    plot=p,
    width=widthpx/dpi, height=heightpx/dpi, units='in', device='png',
    dpi=dpi, bg='grey50'
  )
  image <- png::readPNG(sprintf('~/Downloads/ray-anim/img-%04d.png', 1))
  yz_rect(
    zwidth=floor.mult*width, ywidth=wall.mult*width,
    z=-floor.mult*width/2, y=wall.mult*width/2,
    material=lambertian(image=image),
    angle=c(0, -90, 0), #flipped=TRUE
  )
}

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
      )
    )
  }
  scn
}
