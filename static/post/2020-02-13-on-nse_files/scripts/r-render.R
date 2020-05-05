# source('static/post/2020-02-13-on-nse_files/scripts/r-render.R', echo=TRUE)
library(rayrender)

# blue <- "#1e64b6"
blue <- "#012060"
# gray <- "grey50"
gray <- "grey20"

off <- 1.05

epr <- extruded_polygon(
  xy.coords(allx, ally),
  holes=length(outx) + 1L,  # for some reason index off by 1 relative to decido
  top=.1, bottom=-.1,
  material=diffuse(color=blue),
  plane='xy',
  angle=c(0, 180, -90), order_rotation=c(3, 2, 1),
  material_id=1,
  x=-off
)
epr2 <- extruded_polygon(
  xy.coords(allx, ally),
  holes=length(outx) + 1L,  # for some reason index off by 1 relative to decido
  top=.1, bottom=-.1,
  material=diffuse(color=gray),
  plane='xy',
  angle=c(0, 0, -90), order_rotation=c(3, 2, 1),
  material_id=10,
  x=off
)
eph <- extruded_polygon(
  xy.coords(hulax, hulay),
  holes=length(hbzsp[[1]][[1]])+1,
  top=0.05, bottom=-0.05,
  material=diffuse(color=gray),
  plane='xy',
  angle=c(0, 180, -90), order_rotation=c(3, 2, 1),
  material_id=2,
  x=-off
)
eph2 <- extruded_polygon(
  xy.coords(hulax, hulay),
  holes=length(hbzsp[[1]][[1]])+1,
  top=0.05, bottom=-0.05,
  material=diffuse(color=blue),
  plane='xy',
  angle=c(0, 0, -90), order_rotation=c(3, 2, 1),
  material_id=20,
  x=off
)
l <- sphere(x=2, y=5, z=5, radius=.25, material=light(intensity=200*6))
l2 <- add_object(
  xy_rect(z=10, material=light(intensity=1.5), xwidth=20, ywidth=20),
  xz_rect(y=10, material=light(intensity=1.5), xwidth=20, zwidth=20, flipped=TRUE)
)
f <- xz_rect(y=0, xwidth=4, zwidth=10, material=diffuse(color='white'))
b <- xy_rect(
  z=-4, y=2.5, ywidth=10, xwidth=10, material=diffuse(color='white')
)
cyl <- cylinder(
  phi_min=270, phi_max=360, angle=c(0, 0, 90), 
  length=10, y=1, z=-3,
  # material=diffuse(checkercolor='red', checkerperiod=.5)
)
scene <- dplyr::bind_rows(
  epr, epr2,
  eph, eph2,
  l, l2,
  f, b, cyl
)
w <- h <- 800
s <- 1000
bg <- '#FFFFFF'

render_scene(
  scene, width=w*2, height=h, samples=s,
  fov=13, 
  # fov=40, 
  # lookfrom=c(10, 0, -3),
  lookfrom=c(0, 2, 5),
  lookat=c(0, 0.50, 0),
  clamp_value=5,
  filename='~/Downloads/Rs/two.png',
  aperture=0
)
render_scene(
  dplyr::bind_rows(
    group_objects(add_object(epr, eph), group_translate=c(.6, 0, 0)),
    l, l2, f, b, cyl
  ),
  width=w, height=h, samples=s,
  fov=13, 
  # fov=40, 
  # lookfrom=c(10, 0, -3),
  lookfrom=c(0, 2, 5),
  lookat=c(0, 0.50, 0),
  clamp_value=5,
  aperture=0,
  filename='~/Downloads/Rs/one-1.png'
)
render_scene(
  dplyr::bind_rows(
    group_objects(add_object(epr2, eph2), group_translate=c(-.5, 0, 0)),
    l, l2, f, b, cyl
  ),
  width=w, height=h, samples=s,
  fov=13, 
  # fov=40, 
  # lookfrom=c(10, 0, -3),
  lookfrom=c(0, 2, 5),
  lookat=c(0, 0.50, 0),
  clamp_value=5,
  aperture=0,
  filename='~/Downloads/Rs/one-2.png'
)

go <- group_objects(dplyr::bind_rows(epr, epr2, eph, eph2))
go2 <- group_objects(go, group_translate=c(0, 2.05, 0), group_angle=c(180,180,0))

l1 <- sphere(x=1, y=2, z=7, radius=.25, material=light(intensity=200*6))
l22 <- add_object(
  xy_rect(z=10, material=light(intensity=.2), xwidth=20, ywidth=20),
  xz_rect(y=10, material=light(intensity=2), xwidth=20, zwidth=20, flipped=TRUE)
)
scene2 <- dplyr::bind_rows(go, go2, f, b, cyl, l1, l22)

render_scene(
  scene2, width=w * 2, height=h * 2, samples=s,
  fov=26,
  # fov=40, 
  # lookfrom=c(10, 0, -3),
  lookfrom=c(0, 2, 5),
  lookat=c(0, 1.025, 0),
  clamp_value=5,
  aperture=0,
  filename='~/Downloads/Rs/four.png'
  # filename=mk_name()
)



