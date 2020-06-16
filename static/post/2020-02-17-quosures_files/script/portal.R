source('static/script/mesh-viz/viz-lib.R')

library(svgchop)
# url <- 'https://www.r-project.org/logo/Rlogo.svg'
url <- '~/Downloads/rlang/rlang2.svg'

library(rayrender)
gold <- microfacet(
  roughness=0.1, eta=c(0.216,0.42833,1.3184), kappa=c(3.239,2.4599,1.8661)
)
zz <- parse_paths(url)
ww <- interp_paths(zz, normalize=TRUE)
paths <- lapply(
  ww,
  function(x) {
    res <- x[['d']]
    res[[1]] <- res[[1]] - .5
    res[[2]] <- res[[2]] - .5
    res
  }
)

extrude_path <- function(x, ...) {
  extruded_polygon(
    x, holes=if(length(attr(x, 'starts'))) attr(x, 'starts'), ...
  )
}
# Hex is a mess, we need to clean it up

hex <- paths[[length(paths)]]
outer <- c(3, 6, 7, 10, 13, 14, 3)
hex <- hex[c(outer, 17:nrow(hex)),]
attr(hex, 'starts') <- 8


objs <- dplyr::bind_rows(
  lapply(
    paths[-c(1, length(paths))],
    extrude_path, material=gold, top=.1
  ),
  extrude_path(hex, material=diffuse('gray30'), top=.1),
  bag
)
bricks <- dplyr::bind_rows(
  xz_rect(y=.2, xwidth=.25, zwidth=.25, material=gold)
)
inside_light <- sphere(1, 1, -1.5, radius=.2, material=light(intensity=200))

bg <- '#FFFFFF'
render_scene(
  dplyr::bind_rows(
    group_objects(objs, group_angle=c(-90,0,0), group_translate=c(0,.5,0)),
    bricks,
    sphere(z=5, y=2, x=5, radius=3, material=light(intensity=5)),
    sphere(radius=10, material=diffuse(), flipped=TRUE),
    inside_light
  ),
  filename=next_file("~/Downloads/rlang/imgs/img-"),
  lookfrom=c(0, .5, 1),
  lookat=c(0,.5,0),
  samples=50,
  clamp_value=5,
  fov=60,
  # fov=40,
  aperture=0
)
# objs <- dplyr::bind_rows(
#   Map(
#     extrude_path, rev(paths), top=c(.1, .05) ,
#     bottom=c(-.1,-.05), material=list(gold)
#   )
# )

# Generate the Rlang pocket.  Want two sets of hex coordinates, the front, back,
# and want to generate triangles for the back and sides.  Need to compute from
# our viewpoint and project out the back hexagon.
#
# Challenge, compute what elements in our full scene are actually inside.

h2 <- hex[1:7,]

# @param depth how far from the hex edge to the corresponding vertex in the back
#   hexagon.
# @param y numeric(1L) y coord of original hexagon parallel to x-z plane
# @param obs the observer coordinates (x,y,z)
# @param hex the coordinates of the hex, hex assumed closed, first coordinate is
#   taken to be X, second Z, and the Y value is assumed to be 0.

comp_inside <- function(hex, y, obs, depth, material=diffuse(color='red')) {
  vetr::vetr(
    structure(list(numeric(7), numeric(7)), class='data.frame'),
    numeric(1),
    numeric(3),
    numeric(1)
  )
  hex.in <- rbind(hex[[1]], 0, hex[[2]])
  vecs <- hex.in - obs
  vecs.n <- vecs / sqrt(colSums(vecs^2)) * depth
  hex.out <- hex.in + vecs.n

  i <- seq_len(ncol(vecs.n) - 1L)
  ii <- seq_len(ncol(vecs.n) - 1L) + 1L   # i + 1
  area_s <- sum(hex.in[1,i] * hex.in[3,ii] - hex.in[1,ii] * hex.in[3,i]) / 2

  ccw <- area_s >= 0  # treat degenerates as counter-clockwise

  # code adapted from rayrender

  sides <- unlist(
    lapply(
      seq_len(ncol(vecs.n) - 1L),
      function(i) {
        list(
          triangle(
            v1=hex.in[,i], v2=hex.out[,i], v3=hex.out[,i+1],
            material = material, reversed = ccw
          ),
          triangle(
            v1=hex.in[,i], v2=hex.out[,i+1], v3=hex.in[,i+1],
            material = material, reversed = ccw
          )
    ) } ),
    recursive=FALSE
  )
  back.idx <- matrix(decido::earcut(hex), 3)
  back.tris <- lapply(
    split(back.idx, col(back.idx)),
    function(i)
      triangle(
        hex.out[,i[1]], hex.out[,i[2]], hex.out[,i[3]], 
        material=material
      )
  )
  dplyr::bind_rows(c(sides, back.tris))
}
bag <- comp_inside(h2, 0, c(0, 1, 0), 2, diffuse(color='grey5'))
render_scene(
  bag, lookfrom=c(0,1,0.0000001), lookat=c(0,-2,0), aperture=0,
  samples=100, fov=30
)



