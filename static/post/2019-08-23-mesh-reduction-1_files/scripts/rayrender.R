library(rayrender)

set.seed(1220)
x <- runif(10)
x.width <- 1
x.off <- seq(-x.width / 2, x.width/2, length.out=length(x))

floor.mult <- 2
scene <- xz_rect(
  xwidth=floor.mult*x.width, 
  zwidth=floor.mult*x.width,
  z=floor.mult*x.width/2,
  y=0,
  # material=lambertian(checkercolor='black', checkerperiod=x.width/length(x))
  material=lambertian()
)
# scene <- generate_ground(material = lambertian())
for(i in seq_along(x)) {
  scene <- scene %>% add_object(
    cube(
      x=x.off[i], y=x[i] / 2, z=0,
      xwidth=x.width / length(x),
      zwidth=x.width / length(x),
      ywidth=x[i],
      material=dielectric(color='red'),
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
  final, parallel = TRUE, width = 200, height = 200, samples = 200,
  lookfrom=c(0,1,-2),
  lookat=c(0,0,1),
  fov=20,
  ambient_light=FALSE,
  aperture=.0
)

generate_cornell() %>%
  add_object(
    xz_rect(
      x = 555/2, y = 0, z = 555/2, xwidth = 200, zwidth = 200,
      material = lambertian())
   ) %>%
   render_scene(lookfrom = c(278, 278, -800) ,lookat = c(278, 278, 0), fov = 40, 
   ambient_light = FALSE, samples = 400, parallel = TRUE, clamp_value = 5, width=200, height=200)

scene <- add_object(
  scene,
  cube(
    x=0, z=.05, y=.2, ywidth=.4, xwidth=.1, zwidth=.1,
    angle=c(0, 22.5, 0),
    material=dielectric(color='red')
) )

