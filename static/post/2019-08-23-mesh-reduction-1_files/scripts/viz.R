source('static/post/2019-08-23-mesh-reduction-1_files/scripts/rtin2.R')
source('static/post/2019-08-23-mesh-reduction-1_files/scripts/rtin-vec.R')

# - Plot Helper Tools ----------------------------------------------------------
#

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
# Mx to mesh

map <- matrix(c(0, 0, 0, 0, 1, 0, 0, 1, 0), nrow=3)
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

ids_to_xyz <- function(tris, map, scale) {
  ids <- unlist(tris)
  y <- (ids - 1) %% dim(map)[1]
  x <- (ids - 1) %/% dim(map)[1]
  z <- map[ids]

  x <- x / (dim(map)[1] - 1) * scale[1]
  y <- y / (dim(map)[2] - 1) * scale[2]
  z <- (z - min(z)) / (diff(range(z))) * scale[3]
  list(x, y, z)
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
tris_to_obj <- function(tris, map, scale=c(1, 1, 1)) {
  dat <- ids_to_xyz(tris, map, scale)
  x <- dat[['x']]; y <- dat[['y']]; z <- dat[['z']]

  # need these ordered counterclockwise; for each triangle start with leftmost
  # (lowest x), then compute the angle between that vertex and the remaining two
  # to decide how to order the remaining two.

  tri.id <- rep(seq_len(length(ids)/3), each=3)
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

  v.ids <- matrix(seq_along(ids), 3)
  v.chr <- paste('v', xo2, yo2, zo2, collapse='\n')
  f.chr <- paste('f', v.ids[1,], v.ids[2,], v.ids[3,], collapse='\n')
  paste0(c(v.chr, f.chr), collapse='\n')
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
  tris, map, scale=c(1, 1, 1), material=lambertian(), radius=1,
  angle=c(0,0,0), translate=c(0,0,0)
) {
  xyz_to_seg(ids_to_xyz(tris, map, scale), material, radius, angle, translate)
}
mesh_to_seg <- function(
  mesh, map, scale=c(1, 1, 1), material=lambertian(), radius=1,
  angle=c(0,0,0), translate=c(0,0,0)
) {
  xyz_to_seg(mesh_to_xyz(mesh, map, scale), material, radius, angle, translate)
}

stop('loaded')

# - Test it Out ----------------------------------------------------------------

f <- tempfile()
f2 <- tempfile()
f3 <- tempfile()

vsq <- matrix(min(volcano), 65, 65)
vsq[1:65, 3:63] <- volcano[1:65,1:61]

map <- vsq
errors <- compute_error(map)
tol <- diff(range(map)) / 5
# m2 <- extract_geometry(errors, tol)
tris <- extract_mesh2(errors, tol)
tol <- diff(range(map)) / 2
tris3 <- extract_mesh2(errors, tol)
# plot_tri_ids(tris, dim(errors))

# rayrender a mesh in list format

mesh.tri <- mx_to_mesh(vsq)
mesh.tri.s <- scale_mesh(mesh.tri)

mesh.obj <- mesh_to_obj(mesh.tri.s)
writeLines(mesh.obj, f)
mesh.obj.2 <- tris_to_obj(tris, map)
writeLines(mesh.obj.2, f2)
mesh.obj.3 <- tris_to_obj(tris3, map)
writeLines(mesh.obj.3, f3)

library(rayrender)

scn <- sphere(
  y=4, z = 2, x = 0, radius = .1,
  material = lambertian(lightintensity = 1000, implicit_sample = TRUE)
)
scn <- add_object(
  scn,
  group_objects(
    add_object(
      mesh_to_seg(mesh.tri.s, map, radius=.005, material=lambertian(color='red')),
      obj_model(filename=f, material=lambertian(color='grey50'))
    ),
    group_angle=c(90, 90, 0), group_translate=c(-1.75, 0.05, -.5)
) )
scn <- add_object(
  scn,
  group_objects(
    add_object(
      tris_to_seg(tris, map, radius=.005, material=lambertian(color='red')),
      obj_model(filename=f2, material=lambertian(color='grey50'))
    ),
    group_angle=c(90, 90, 0), group_translate=c(-0.5, 0.05, -.5)
) )
# scn <- add_object(
#   scn,
#   obj_model(
#     filename=f3, x=1.75, y=0, z=0.5, angle=c(90, 90, 0),
#     material=lambertian(color='grey50')
#   )
# )
scn <- add_object(
  scn, xz_rect(xwidth=5, zwidth=2, material=lambertian(color='white'))
)
render_scene(
  scn, 
  # width=400, height=400, samples=400,
  width=400, height=200, samples=200,
  lookfrom=c(0, 4, 1),
  lookat=c(0, 0.25, 0),
  aperture=0, fov=0,
  ortho_dimensions=c(4,2)
  # backgroundimage='~/Downloads/blank.png'
)

library(rgl)
par3d(windowRect=c(20,20,400,400))
mesh.obj.3 <- tris_to_obj(tris3, map)
writeLines(mesh.obj.3, f3)
plot3d(readOBJ(f3), col='grey50')

render_scene(
  bind_rows(
    sphere(x=0, y=5, z=5, material=lambertian(lightintensity=300)),
    xz_rect(xwidth=10, zwidth=10, y=-1, material=lambertian(checkercolor='grey10')),
    cylinder(
      length=1, x=c(-.5), radius=.125, material=metal(color='red')
    ),
    cylinder(
      length=1, x=c(0), radius=.125, material=metal(color='red'),
      angle=c(45, 45, 45), order_rotation=c(1, 2, 3)
    ),
    cylinder(
      length=1, x=c(.5), radius=.125, material=lambertian(color='red'),
      angle=c(45, 45, 45), order_rotation=c(2, 1, 3)
    ),
    sphere(
      x=c(-.5,0,.5), y=0, z=0, radius=.2, material=dielectric(color='green')
    )
  ),
  lookfrom=c(0, 2, 4),
  width=200, height=200, samples=400
)

xx <- tris_to_seg(
  tris3, map, radius=0.05, material=lambertian(color='red')
)
render_scene(
  bind_rows(
    xx,
    xz_rect(xwidth=5, zwidth=5, y=-1, material=lambertian(color='grey50')),
    xz_rect(
      xwidth=5, zwidth=5, y=1.5, angle=c(-90,0,0),
      material=lambertian(color='grey50')
    ),
    xz_rect(
      xwidth=5, zwidth=5, y=1.5, x=-2, angle=c(0,0,90),
      material=lambertian(color='grey50')
    ),
    sphere(material=lambertian(lightintensity=500), x=0, z=2, y=4, radius=.5)
  ),
  width=200, height=200, samples=200,
  lookat=c(0.5, 0, 0),
  lookfrom=c(0, 4, 4),
  aperture=0,
  fov=45
)


