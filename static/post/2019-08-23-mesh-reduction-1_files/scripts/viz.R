library(vetr)

source('static/post/2019-08-23-mesh-reduction-1_files/scripts/rtin2.R')
source('static/post/2019-08-23-mesh-reduction-1_files/scripts/rtin-vec.R')

# - Plot Helper Tools ----------------------------------------------------------

## Rescale data to a range from 0 to `range` where `range` in (0,1]
rescale <- function(x, range=1, center=0.5)
  ((x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))) * range +
   (1 - range) * center

## Prepare a plot with a particular aspect ratio
plot_new <- function(
  x, y, xlim=c(0,1), ylim=c(0,1),
  par.args=list(mai=numeric(4L), xaxt='n', yaxt='n', xaxs='i', yaxs='i')
) {
  if(length(par.args)) do.call(par, par.args)
  plot.new()
  plot.window(
    xlim, ylim, asp=diff(range(y, na.rm=TRUE))/diff(range(x, na.rm=TRUE))
) }
plot_tri_ids <- function(tri, dim, new=TRUE) {
  ids <- rbind(do.call(cbind, tri), NA) - 1L
  x <- ids %% dim[1]
  y <- ids %/% dim[1]
  if(new) plot_new(x, y)
  polygon(x/(dim[1] - 1), y/(dim[2]-1), col='#DDDDDD', border='#444444')
}
plot_points_ids <- function(points.ids, dim, cex=1, col='red') {
  ids <- points.ids - 1L
  x <- ids %% dim[1]
  y <- ids %/% dim[1]
  x0 <- seq_len(dim[1]) - 1L
  y0 <- seq_len(dim[2]) - 1L
  points(
    rep(x0/max(x0), each=dim[2]), rep(y0/max(y0), dim[1]),
    pch=16, col='black', cex=0.5
  )
  points(x/max(x0), y/max(y0), pch=16, col=col, cex=cex)
}
# Plotting extract geom

if(FALSE) {
  raw <- extract_geometry(errors, tol)

  points(rescale(which(errors > tol, arr.ind=TRUE)[,2:1] - 1), pch=19, col='red')

  plot_ex_geom <- function(raw, errors) {
    xs <- matrix(raw %% nrow(errors), 3)
    ys <- matrix(raw %/% ncol(errors), 3)

    plot_new(xs, ys)
    polygon(
      rescale(rbind(xs, NA)), rescale(rbind(ys, NA)),
      col='#DDDDDD', border='#444444'
    )
  }
}
# map <- matrix(c(0, 0, 0, 0, 1, 0, 0, 1, 0), nrow=3)

# Mx to data frame, unlike most of the funs this flip y/z to align with the
# default rayrender camera viewpoint.

mx_to_df <- function(mx, scale=rep(1, 3)) {
  df <- reshape2::melt(mx)
  names(df) <- c('x', 'z', 'y')
  df[] <- Map(rescale, df, scale, numeric(3))
  df[, c('x', 'y', 'z')]
}
# Turn an elevation matrix into a triangle "mesh" stored in list matrix format

mx_to_mesh <- function(mx) {
  nr <- dim(mx)[1]
  nc <- dim(mx)[2]
  idx.raw <- matrix(seq_along(mx), nr, nc)

  idx.tile <- list(
    v1=c(idx.raw[-nr, -nc]), v2=c(idx.raw[-nr,  -1]),
    v3=c(idx.raw[ -1,  -1]), v4=c(idx.raw[ -1, -nc])
  )
  vl <- list(x=c(col(mx)), y=c(row(mx)), z=c(mx), t=rep(0, length(mx)))

  mesh.tile <- matrix(
    list(), nrow=length(idx.tile), ncol=length(vl),
    dimnames=list(names(idx.tile), names(vl))
  )
  ## Fill it with the correctly subset volcano data
  for(i in names(idx.tile))
    for(j in names(vl))
      mesh.tile[[i,j]] <- vl[[j]][idx.tile[[i]]]

  mesh.tile

  vertex.blue <- 1:3
  vertex.green <- c(3,4,1)
  mesh.tri <- Map('c', mesh.tile[vertex.blue,], mesh.tile[vertex.green,])
  dim(mesh.tri) <- c(3, 4)
  dimnames(mesh.tri) <- list(head(rownames(mesh.tile), -1), colnames(mesh.tile))
  mesh.tri
}
scale_mesh <- function(mesh, scale=rep(1, 4)) {
  stopifnot(identical(ncol(mesh), length(scale)))
  res <- mesh
  for(i in seq_len(ncol(mesh))) {
    rng <- range(unlist(mesh[,i]))
    for(j in seq_len(nrow(mesh))) {
      res[[j, i]] <- (mesh[[j, i]] - rng[1]) / diff(rng) * scale[i]
    }
  }
  res
}
# Convert list-matrix mesh into obj text format

mesh_to_obj <- function(mesh) {
  # first step is convert to array, drop texture for now

  mesh.arr <- array(unlist(mesh[1:3, 1:3]), c(length(mesh[[1]]), 3, 3))
  mesh.v <- matrix(aperm(mesh.arr, c(3, 2, 1)), nrow=3)
  v.chr <- paste('v', mesh.v[1,], mesh.v[2,], mesh.v[3,], collapse="\n")

  # faces are easy because we've repeated the vertices so no shared vertices

  mesh.f <-matrix(seq_len(length(mesh[[1]]) * 3), nrow=3)
  f.chr <- paste('f', mesh.f[1,], mesh.f[2,], mesh.f[3,], collapse="\n")

  paste0(c(v.chr, f.chr), collapase="\n")
}
# Return format from extract_mesh2, the vertices are returned in triangle order
# (i.e. first three are first triangle, next 3 are second triangle, etc.)

ids_to_xyz <- function(tris, map, scale, flatten=FALSE) {
  ids <- unlist(tris)
  if(!length(ids)) {
    # minimal triangle
    nr <- nrow(map)
    tr <- length(map) - nrow(map) + 1L
    ids <- c(1L, nr, tr, tr, nr, length(map))
  }
  y <- (ids - 1) %% dim(map)[1]
  x <- (ids - 1) %/% dim(map)[1]
  z <- map[ids]

  x <- x / (dim(map)[1] - 1) * scale[1]
  y <- y / (dim(map)[2] - 1) * scale[2]
  z <- if(flatten) numeric(length(z))
       else (z - min(z)) / (diff(range(z))) * scale[3]
  list(x=x, y=y, z=z)
}
mesh_to_xyz <- function(mesh, map, scale) {
  mesh.arr <- array(unlist(mesh[1:3, 1:3]), c(length(mesh[[1]]), 3, 3))
  mesh.v <- matrix(aperm(mesh.arr, c(3, 2, 1)), nrow=3)
  res <- list(x=mesh.v[1,], y=mesh.v[2,], z=mesh.v[3,])
  Map(
    function(x, s) (x - min(x)) / diff(range(x)) * s,
    res, scale
  )
}
# Generates vertices and triangles from id-based triangle coordinates
# Attempts to order vertices so that the normals are "up", which really only
# works properly so long as surfaces are no steeper than vertical.
#
# Maybe a better way would be to find the center of the whole object and use
# that as the reference.

tris_to_obj <- function(
  tris, map, scale=c(1, 1, 1), flatten=FALSE, width=0, bevel=45
) {
  dat <- ids_to_xyz(tris, map, scale, flatten)
  x <- dat[['x']]; y <- dat[['y']]; z <- dat[['z']]

  # need these ordered counterclockwise; for each triangle start with leftmost
  # (lowest x), then compute the angle between that vertex and the remaining two
  # to decide how to order the remaining two.

  tri.id <- rep(seq_len(length(x)/3), each=3)
  o <- order(
    tri.id,
    x != rep(
      matrix(x, nrow=3)[
        cbind(
          max.col(matrix(-x, ncol=3, byrow=TRUE), ties.method='first'),
          seq_len(length(x)/3)
        )
      ],
      each=3
    )
  )
  xo <- matrix(x[o], 3)
  yo <- matrix(y[o], 3)

  m1 <- (yo[2,] - yo[1,]) / (xo[2,] - xo[1,])
  m2 <- (yo[3,] - yo[1,]) / (xo[3,] - xo[1,])

  mo <- matrix(logical(length(xo)), 3)
  mo[2, m1 > m2] <- TRUE

  o2 <- order(tri.id, mo)
  xo2 <- xo[o2]
  yo2 <- yo[o2]
  zo2 <- z[o][o2]

  # Generate a second set of vertices that is just a wee bit smaller than the
  # first

  xo21 <- xo2 * .95 + .025
  yo21 <- yo2 * .95 + .025
  zo21 <- zo2 * .95 + .025

  o3 <- order(tri.id, rep_len(c(1L, 3L, 2L), length(tri.id)))
  xo3 <- xo21[o3]
  yo3 <- yo21[o3]
  zo3 <- zo21[o3]

  v.ids <- matrix(seq_len(length(x) * 2), 3)
  v.chr <- paste('v', c(xo2, xo3), c(yo2, yo3), c(zo2, zo3), collapse='\n')
  f.chr <- paste('f', v.ids[1,], v.ids[2,], v.ids[3,], collapse='\n')
  paste0(c(v.chr, f.chr), collapse='\n')
}
# We call shards a two triangle sandwich with the faces closed off by bevels,
# which would look kind of like an arrowhead.
#
# Given x-y-z coordinates of triangles, a depth, and bevel angle, return
# the corresponding coordinates of the shard / arrow head.
#
# x-y-z coordinates are ordered by triangle (i.e. first 3 of each are first
# triangle, and so on).  The vertex order within each triangle is considered
# important and will be used to derive the vertex orders for the new faces.

xyz_to_shard <- function(xyz, depth, bevel) {
  vetr(
    list(x=numeric(), y=numeric(), z=numeric()) && length(unique(lengths(.))) == 1,
    NUM.1.POS, NUM.1.POS && . <= 90
  )
  # this is inefficient...
  x <- matrix(xyz[['x']], 3)
  y <- matrix(xyz[['y']], 3)
  z <- matrix(xyz[['z']], 3)
  v1 <- cbind(x[1,], y[1,], z[1, ])
  v2 <- cbind(x[2,], y[2,], z[2, ])
  v3 <- cbind(x[3,], y[3,], z[3, ])

  # Compute how much we need to scale the faces relative to the bevel edge,
  # pick the first vertex as the reference point, and compute delta distance
  # from vertex to barycenter accounting for bevel, and assume that the ratio
  # of vertex to barycenter is the same for all other vertices.
  #       ^
  #      /|\
  #     / | \        We're trying to compute the distance between the ^ for
  #    /  ^  \       arbitrary triangles in 3D
  #   /  / \  \
  #  /  /   \  \

  bl <- (depth / 2) / tan(bevel / 180 * pi)
  V <- v2 - v1
  W <- v3 - v1

  # cos ab = V . W / |V||W|
  cos.ab <- (rowSums(V * W) / (sqrt(rowSums(V ^ 2)) * sqrt(rowSums(V ^ 2))))
  delta <- bl / (cos.ab / 2)

  # barycenter coordinates and scaling factor
  vb <- (v1 + v2 + v3) / 3
  tri.radius <- sqrt(rowSums((v1 - vb) ^ 2))
  scale <- (tri.radius - delta) / tri.radius
  if(any(scale <= 0)) {
    warning("Bevel offset exceeds triangle side for ", which(scale <= 0)[1])
  }
  # recompute new scaled coordinates
  v1s <- vb + (v1 - vb) * scale
  v2s <- vb + (v2 - vb) * scale
  v3s <- vb + (v3 - vb) * scale

  # We now need the two reduced facets, along with all the triangles to seal the
  # bevel faces.  We need four triangles per side.  Start by offsetting the top
  # and bottom faces along the normals by the depth of the shard.

  N <- cbind(
    V[,2] * W[,3] - V[,3] * W[,2],
    V[,3] * W[,1] - V[,1] * W[,3],
    V[,1] * W[,2] - V[,2] * W[,1]
  )
  n <- N / sqrt(rowSums(N^2))

  v1t <- v1s + n * (depth / 2)
  v2t <- v2s + n * (depth / 2)
  v3t <- v3s + n * (depth / 2)

  v1b <- v1s - n * (depth / 2)
  v2b <- v2s - n * (depth / 2)
  v3b <- v3s - n * (depth / 2)

      triangle(v2m, v3m, v3t, material=mat),
      triangle(v3t, v2t, v2m, material=mat),
      triangle(v2m, v2b, v3b, material=mat),
      triangle(v3b, v3m, v2m, material=mat),



      triangle(v1t, v2t, v3t, material=mat),
      triangle(v1b, v2b, v3b, material=mat, flipped=TRUE),


      triangle(v1m, v2m, v2t, material=mat),
      triangle(v2t, v1t, v1m, material=mat),
      triangle(v1m, v1b, v2b, material=mat),
      triangle(v2b, v2m, v1m, material=mat),

      triangle(v3t, v3m, v1m, material=mat),
      triangle(v1m, v1t, v3t, material=mat),
      triangle(v3b, v1b, v1m, material=mat),
      triangle(v1m, v3m, v3b, material=mat)

}
xyz_to_seg <- function(xyz, material, radius, angle, translate) {
  coords <- array(
    unlist(xyz), c(3, length(xyz[[1]]) / 3, 3),
    dimnames=list(paste0('v', 1:3), NULL, c('x', 'y', 'z'))
  )
  a <- rbind(coords[1,,], coords[2,,], coords[3,,])
  b <- rbind(coords[2,,], coords[3,,], coords[1,,])
  dir <- sqrt(rowSums(a^2)) < sqrt(rowSums(b^2))
  starts <- a
  ends <- b
  starts[!dir,] <- b[!dir]
  ends[!dir,] <- a[!dir]

  segdat <- unique(cbind(starts, ends))

  group_objects(
    dplyr::bind_rows(
      lapply(
        seq_len(nrow(segdat)),
        function(i) {
          segment(
            start=segdat[i, 1:3], end=segdat[i, 4:6],
            radius=radius, material=material
          )
        }
    ) ),
    group_angle=angle, group_translate=translate
  )
}
tris_to_seg <- function(
  tris, map, scale=c(1, 1, 1), material=diffuse(), radius=1,
  angle=c(0,0,0), translate=c(0,0,0), flatten=FALSE
) {
  xyz_to_seg(
    ids_to_xyz(tris, map, scale, flatten), material, radius, angle, translate
  )
}
mesh_to_seg <- function(
  mesh, map, scale=c(1, 1, 1), material=diffuse(), radius=1,
  angle=c(0,0,0), translate=c(0,0,0)
) {
  xyz_to_seg(mesh_to_xyz(mesh, map, scale), material, radius, angle, translate)
}

stop('loaded')

# - Test it Out ----------------------------------------------------------------

# seg.0 <- mesh_to_seg(mesh.tri.s, map, radius=seg.rad, material=diffuse(color='red'))

library(rayrender)

seg2 <- tris_to_seg(tris2, map, radius=seg.rad, material=seg.mat)
seg3 <- tris_to_seg(tris3, map, radius=seg.rad, material=seg.mat)


# ------------------------------------------------------------------------------
# - Recorded  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# - Simplest Cases -------------------------------------------------------------

f <- tempfile()
map <- matrix(
  c(
     0, .1,   0,
    .5,  1, .05,
    .1, .2, .05
  ), 3, byrow=TRUE
)
set.seed(1221)
map <- matrix(runif(25), 5)
seg.rad <- .03
seg.mat <- metal(color='gold')
zoff <- .5

errors <- compute_error(map)
tris0 <- extract_mesh2(errors, .0)
seg0 <- tris_to_seg(tris0, map, radius=seg.rad, material=seg.mat)
mesh.obj <- tris_to_obj(tris0, map)
writeLines(mesh.obj, f)

map.df <- mx_to_df(map)
spheres <- lapply(
  seq_len(nrow(map.df)),
  function(i)
    sphere(
      map.df[i, 'x'], map.df[i, 'y'], map.df[i, 'z'], radius=.025,
      material=metal()
) )
scn <- dplyr::bind_rows(
  sphere(
    y=8, z = 4, x = 0, radius = 1,
    material = diffuse(lightintensity = 200, implicit_sample = TRUE)
  ),
  group_objects(
    obj_model(filename=f, material=dielectric(color='#AAAAFF')),
    group_angle=c(90, -90, 0), group_translate=c(-.5, 0, -.5),
    pivot_point=numeric(3)
  ),
  group_objects(
    dplyr::bind_rows(
      segment(
        unlist(subset(map.df, x==1& z==0)), unlist(subset(map.df, x==0& z==0)),
        material=seg.mat, radius=0.025
      ),
      segment(
        unlist(subset(map.df, x==0& z==0)), unlist(subset(map.df, x==0& z==1)),
        material=seg.mat, radius=0.025
      ),
      segment(
        unlist(subset(map.df, x==1& z==0)), unlist(subset(map.df, x==0& z==1)),
        material=seg.mat, radius=0.025
      )
    ),
    group_angle=c(0, 0, 0), 
    group_translate=c(-.5, 0, -.5)
  ),
  group_objects(
    dplyr::bind_rows(spheres),
    group_angle=c(0, 0, 0), 
    group_translate=c(-.5, 0, -.5)
  ),
  xz_rect(
    y=-1, xwidth=5, zwidth=5, material=diffuse(
      color='grey50'
      # color='white', checkercolor='green', checkerperiod=0.25
    )
  ),
  xz_rect(
    y=1.5, z=-2.5, xwidth=5, zwidth=5, material=diffuse(
      color='white', checkercolor='blue', checkerperiod=0.25
    ),
    angle=c(-90, 0, 0)
  )
)
rez <- 400
render_scene(
  scn,
  # ambient_light=TRUE,
  width=rez, height=rez, samples=rez,
  lookfrom=c(.5, 4, .5),
  lookat=c(0, .5, 0),
  aperture=0, fov=0, 
  ortho_dimensions=c(1.5,1.5),
  clamp=3,
  file='~/Downloads/mesh-viz/test-2.png',
  # backgroundimage='~/Downloads/blank.png',
  camera_up=c(1,0,0)
)

shift <- 0.05/sin(atan(0.5))
baryx <- 0
baryz <- (2 * -.5 + .5) / 3
shift.ratio <- shift / (.5 - baryz)
sr <- 1 + shift.ratio
c(-.5,.5) * (1 + shift.ratio)

v1t <- c(-.5,    .50,    -.5)
v2t <- c(  0,    .50,     .5)
v3t <- c( .5,    .50,    -.5)

v1m <- c(-.5*sr, .45, -.5*sr)
v2m <- c(     0, .45,  .5*sr)
v3m <- c( .5*sr, .45, -.5*sr)

v1b <- c(-.5,    .40,    -.5)
v2b <- c(  0,    .40,     .5)
v3b <- c( .5,    .40,    -.5)

# mat <- diffuse(color='grey70', checkercolor='black', checkerperiod=.25)
mat <- dielectric(color='#FFAAAA')
scn <- dplyr::bind_rows(
  sphere(
    y=8, z = 4, x = 0, radius = 1,
    material = diffuse(lightintensity = 200, implicit_sample = TRUE)
  ),
  group_objects(
    dplyr::bind_rows(
      triangle(v1t, v2t, v3t, material=mat),
      triangle(v1b, v2b, v3b, material=mat, flipped=TRUE),

      triangle(v2m, v3m, v3t, material=mat),
      triangle(v3t, v2t, v2m, material=mat),
      triangle(v2m, v2b, v3b, material=mat),
      triangle(v3b, v3m, v2m, material=mat),

      triangle(v1m, v2m, v2t, material=mat),
      triangle(v2t, v1t, v1m, material=mat),
      triangle(v1m, v1b, v2b, material=mat),
      triangle(v2b, v2m, v1m, material=mat),

      triangle(v3t, v3m, v1m, material=mat),
      triangle(v1m, v1t, v3t, material=mat),
      triangle(v3b, v1b, v1m, material=mat),
      triangle(v1m, v3m, v3b, material=mat)
    ),
    group_angle=c(0, 0, 22.5)
  ),
  xz_rect(
    y=-1, xwidth=5, zwidth=5, material=diffuse(
      color='white', checkercolor='green', checkerperiod=0.25
    )
  ),
  xz_rect(
    y=1.5, z=-2.5, xwidth=5, zwidth=5, material=diffuse(
      color='white', checkercolor='blue', checkerperiod=0.25
    ),
    angle=c(-90, 0, 0)
  )
)
rez <- 400
render_scene(
  scn,
  # ambient_light=TRUE,
  width=rez, height=rez, samples=rez,
  lookfrom=c(0, 2, 2),
  lookat=c(0, .75, 0),
  aperture=0, fov=45, 
  # ortho_dimensions=c(1.5,1.5),
  clamp=3
  # file='~/Downloads/mesh-viz/simple-1.png'
  # backgroundimage='~/Downloads/blank.png'
)



# - Original With Glass --------------------------------------------------------

f <- tempfile()
f2 <- tempfile()
f3 <- tempfile()

vsq <- matrix(0, 65, 65)
vsq[1:65, 3:63] <- volcano[1:65,1:61]
vsq[1:65, 1:2] <- volcano[1:65, 1]
vsq[1:65, 64:65] <- volcano[1:65, 61]

map <- vsq
errors <- compute_error(map)
elmax <- diff(range(map))
tris0 <- extract_mesh2(errors, elmax/50)
tris2 <- extract_mesh2(errors, elmax/20)
tris3 <- extract_mesh2(errors, elmax/3)
# plot_tri_ids(tris, dim(errors))

# rayrender a mesh in list format

mesh.tri <- mx_to_mesh(vsq)
mesh.tri.s <- scale_mesh(mesh.tri)

# mesh.obj <- mesh_to_obj(mesh.tri.s)
mesh.obj <- tris_to_obj(tris0, map)
writeLines(mesh.obj, f)
mesh.obj.2 <- tris_to_obj(tris2, map)
writeLines(mesh.obj.2, f2)
mesh.obj.3 <- tris_to_obj(tris3, map)
writeLines(mesh.obj.3, f3)

seg.rad <- .0025
seg.mat <- metal(color='gold')

zoff <- +.5

scn <- sphere(
  y=8, z = 4, x = 0, radius = .2,
  material = diffuse(lightintensity = 2000, implicit_sample = TRUE)
)
scn <- add_object(
  scn,
  group_objects(
    # add_object(seg0, obj_model(filename=f, material=diffuse(color='grey50'))),
    # add_object(seg0, obj_model(filename=f, material=dielectric())),
    seg0,
    group_angle=c(90, 90, 0), group_translate=c(-.75, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn,
  group_objects(
    #add_object(seg2, obj_model(filename=f2, material=dielectric())),
    seg2,
    group_angle=c(90, 90, 0), group_translate=c(+0.5, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn,
  group_objects(
    #add_object(seg3, obj_model(filename=f3, material=dielectric())),
    seg3,
    group_angle=c(90, 90, 0), group_translate=c(+1.75, 0, zoff),
    pivot_point=numeric(3)
) )
scn <- add_object(
  scn, xz_rect(xwidth=5, zwidth=5, material=diffuse(color='white'))
)
render_scene(
  scn,
  # width=400, height=400, samples=400,
  width=800, height=300, samples=2000,
  # width=200, height=75, samples=100,
  lookfrom=c(0, 4, 2),
  lookat=c(0, 0, 0),
  aperture=0, fov=0,
  ortho_dimensions=c(4,1.5),
  clamp=3,
  file='~/Downloads/mesh-viz/three-abreast.png'
  # backgroundimage='~/Downloads/blank.png'
)

library(rgl)
par3d(windowRect=c(20,20,400,400))
mesh.obj.3 <- tris_to_obj(tris3, map)
writeLines(mesh.obj.3, f3)
plot3d(readOBJ(f3), col='grey50')

render_scene(
  bind_rows(
    sphere(x=0, y=5, z=5, material=diffuse(lightintensity=300)),
    xz_rect(xwidth=10, zwidth=10, y=-1, material=diffuse(checkercolor='grey10')),
    cylinder(
      length=1, x=c(-.5), radius=.125, material=metal(color='red')
    ),
    cylinder(
      length=1, x=c(0), radius=.125, material=metal(color='red'),
      angle=c(45, 45, 45), order_rotation=c(1, 2, 3)
    ),
    cylinder(
      length=1, x=c(.5), radius=.125, material=diffuse(color='red'),
      angle=c(45, 45, 45), order_rotation=c(2, 1, 3)
    ),
    sphere(
      x=c(-.5,0,.5), y=0, z=0, radius=.2, material=dielectric(color='green')
    )
  ),
  lookfrom=c(0, 2, 4),
  width=200, height=200, samples=400
)

