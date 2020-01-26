source('static/script/mesh-viz/viz-batch-init.R')

# - stacked meshes -------------------------------------------------------------

make_layers <- function(
  errors, tol, colors, sizes, material=metal, xmax=Inf, ymax=Inf
) {
  z.off <- 0.5
  colors <- rep_len(colors, length(tol))
  sizes <- rep_len(sizes, length(tol))
  shifts <- cumsum(sizes) - sizes/2

  meshes <- lapply(tol, extract_mesh2, errors=errors)
  segs <- lapply(
    seq_along(meshes), function(i) {
      writeLines(sprintf('running %d', i))
      mat <- material(color=colors[i])
      tris_to_cube(
        meshes[[i]], map, xwidth=sizes[i], zwidth=sizes[i], material=mat,
        flatten=TRUE
      )
    }
  )
  if(is.finite(xmax) || is.finite(ymax)) {
    segs <- lapply(segs, function(x) dplyr::filter(x, x <= xmax, y <= ymax))
  }
  layers <- lapply(
    seq_along(segs),
    function(i) {
      group_objects(
        segs[[i]],
        group_angle=c(90, 0, 0),
        group_translate=c(-zoff, shifts[i], +zoff),
        pivot_point=numeric(3)
    ) }
  )
}
light.medium <- sphere(
  y=8, z = 0, x = 0, radius = .25,
  material = light(intensity = 500)
)
light.narrow <- sphere(
  y=8, z = 4, x = 0, radius = .2, material = light(intensity=2000)
)
map <- vsq
errors <- rtini::rtini_error(map)

# offset 3 stack

tol <- rev(c(50, 9.99754364417931, 1.9))
err.frac <- tol
# mesh.colors <- c('gold', 'grey65', '#DD4F12')
gold <- '#AA9A00'
metal.col <-  c(gold, 'grey35', '#CC3322')
mesh.colors <- metal.col
floor <- xz_rect(xwidth=15, zwidth=15, material=diffuse(color='white'))

# rez <- 600
# samp <- 400
rez <- 600
samp <- 400
mult <- .45
bg1 <- 102 / mult
bg <- do.call(rgb, c(as.list(rep(bg1, 3)), max=255))
light.wide <- sphere(
  y=10, z = 5, x = -10, radius = .5,
  material = light(intensity = 2 * 10 * 25)
  # material = light(intensity = 8)
)

for(i in seq_along(err.frac)) {
  objs <- make_layers(
    errors, err.frac[i], mesh.colors[i], sizes=seg.rad/3, material=diffuse
  )
  scene <- dplyr::bind_rows(objs, list(light.wide, floor))
  file <- next_file('~/Downloads/mesh-viz/flat-mesh/stacked-')
  render_scene(
    scene, width=rez, height=rez, samples=samp,
    lookfrom=c(0, 2, 0), lookat=c(0, 0, 0), aperture=0, fov=0,
    ortho_dimensions=c(1.05,1.05),
    # ortho_dimensions=c(4,4),
    camera_up=c(1,0,0),
    # ortho_dimensions=c(1.1,1.1),
    clamp=3, ambient=TRUE, backgroundlow=bg, backgroundhigh=bg, file=file
  )
}
objs <- make_layers(
  errors, err.frac, mesh.colors, sizes=seg.rad*c(1.75,1,.25)/3, material=diffuse
)
scene <- dplyr::bind_rows(objs, list(light.wide, floor))
rez <- rez / 6 * 10
samp <- samp

file <- next_file('~/Downloads/mesh-viz/flat-mesh/stacked-')
render_scene(
  scene, width=rez, height=rez, samples=samp,
  lookfrom=c(0, 2, 0), lookat=c(0, 0, 0), aperture=0, fov=0,
  # ortho_dimensions=c(.2,.2),
  camera_up=c(1,0,0),
  ortho_dimensions=c(1.05,1.05),
  clamp=3, ambient=TRUE, backgroundlow=bg, backgroundhigh=bg, file=file
)
file <- next_file('~/Downloads/mesh-viz/flat-mesh/stacked-')
render_scene(
  scene, width=rez, height=rez, samples=samp,
  lookfrom=c(.25, 2, .25), lookat=c(.25, 0, .25), aperture=0, fov=0,
  # ortho_dimensions=c(.2,.2),
  camera_up=c(1,0,0),
  ortho_dimensions=c(.525,.525),
  clamp=3, ambient=TRUE, backgroundlow=bg, backgroundhigh=bg, file=file
)
stop()
# # full 8 stack
#
# err.frac.8 <- rev(elmax/2^(0:7))
# mesh.colors <- gold
# floor <- xz_rect(xwidth=5, zwidth=5, material=lambertian(color='white'))
# objs <- make_layers(errors, err.frac.8, mesh.colors, radius=seg.rad, 3, 0)
# scene <- dplyr::bind_rows(objs, list(light.wide, floor))
#
# render_scene(
#   scene, width=width, height=width, samples=samples,
#   lookfrom=c(0, 2, 0), lookat=c(0, 0, 0), aperture=0, fov=34.5,
#   camera_up=c(1,0,0),
#   clamp=3, file='~/Downloads/mesh-viz/batch-2.png'
# )
# # viridis 8 stack
#
# mesh.colors <- substr(viridisLite::viridis(length(err.frac.8)), 1, 7)
# floor <- xz_rect(xwidth=5, zwidth=5, material=lambertian(color='white'))
# objs <- make_layers(errors, err.frac.8, mesh.colors, radius=seg.rad, 3, 0)
# scene <- dplyr::bind_rows(objs, list(light.wide, floor))
#
# render_scene(
#   scene, width=width, height=width, samples=samples,
#   lookfrom=c(0, 2, 0), lookat=c(0, 0, 0), aperture=0, fov=34.5,
#   camera_up=c(1,0,0),
#   clamp=3, file='~/Downloads/mesh-viz/batch-3.png'
# )
# abreast top
stop()

mesh.colors <- metal.col
mat0 <- lambertian(color=mesh.colors[1])
mat2 <- lambertian(color=mesh.colors[2])
mat3 <- lambertian(color=mesh.colors[3])
tris0 <- extract_mesh2(errors, err.frac[1])
tris2 <- extract_mesh2(errors, err.frac[2])
tris3 <- extract_mesh2(errors, err.frac[3])
seg0f <- tris_to_seg(tris0, map, radius=seg.rad*1.5, material=mat0, flatten=TRUE)
seg2f <- tris_to_seg(tris2, map, radius=seg.rad*1.5, material=mat2, flatten=TRUE)
seg3f <- tris_to_seg(tris3, map, radius=seg.rad*1.5, material=mat3, flatten=TRUE)

scn <- add_object(
  light.wide,
  group_objects(
    seg0f, group_angle=c(90, 90, 0), group_translate=c(-.75, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn,
  group_objects(
    seg2f,
    group_angle=c(90, 90, 0), group_translate=c(+0.5, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn,
  group_objects(
    seg3f,
    group_angle=c(90, 90, 0), group_translate=c(+1.75, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(scn, floor)
render_scene(
  scn, width=width/400*150, height=width, samples=samples,
  lookfrom=c(0, 4, 0), lookat=c(0, 0, 0), aperture=0, fov=0,
  ortho_dimensions=c(1.5,4), camera_up=c(1,0,0),
  clamp=3, file='~/Downloads/mesh-viz/batch-4.png'
)
# abreast side

mat0m <- metal(color=mesh.colors[1])
mat2m <- metal(color=mesh.colors[2])
mat3m <- metal(color=mesh.colors[3])
seg0s <- tris_to_seg(tris0, map, radius=seg.rad, material=mat0m)
seg2s <- tris_to_seg(tris2, map, radius=seg.rad, material=mat2m)
seg3s <- tris_to_seg(tris3, map, radius=seg.rad, material=mat3m)

scn <- add_object(
  light.narrow,
  group_objects(
    seg0s, group_angle=c(90, 90, 0), group_translate=c(-.75, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn,
  group_objects(
    seg2s,
    group_angle=c(90, 90, 0), group_translate=c(+0.5, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn,
  group_objects(
    seg3s,
    group_angle=c(90, 90, 0), group_translate=c(+1.75, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn, xz_rect(xwidth=5, zwidth=5, material=lambertian(color='white'))
)
render_scene(
  scn, width=width, height=width/400*150, samples=samples,
  lookfrom=c(0, 4, 2), lookat=c(0, 0, 0), aperture=0, fov=0,
  ortho_dimensions=c(4,1.5),
  clamp=3, file='~/Downloads/mesh-viz/batch-5.png'
)
