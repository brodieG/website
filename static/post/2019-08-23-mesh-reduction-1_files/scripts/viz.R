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
# Return format from extract_mesh2

tris_to_obj <- function(tris, map, scale=c(1, 1, 1)) {
  ids <- unlist(tris)
  y <- (ids - 1) %% dim(map)[1]
  x <- (ids - 1) %/% dim(map)[1]
  z <- map[ids]

  x <- x / (dim(map)[1] - 1) * scale[1]
  y <- y / (dim(map)[2] - 1) * scale[2]
  z <- (z - min(z)) / (diff(range(z))) * scale[3]

  # need these ordered counterclockwise; for each triangle start with leftmost
  # (lowest x), then compute the angle between that vertex and the remaining two
  # to decide how to order the remaining two.

  o <- order(
    rep(seq_len(length(ids)/3), each=3),
    x !=
    matrix(rep(x, 3), ncol=3)[
      cbind(
        seq_len(length(ids)),
        rep(
          max.col(matrix(-x, ncol=3, byrow=TRUE), ties.method='first'), 
          each=3
      ) )
    ]
  )
  xo <- matrix(x[o], 3)
  yo <- matrix(y[o], 3)

  m1 <- (yo[2,] - yo[1,]) / (xo[2,] - xo[1,])
  m2 <- (yo[2,] - yo[1,]) / (xo[2,] - xo[1,])

  yo <- z[o]

  t.ids <- matrix(seq_along(ids), 3)
  v.chr <- paste('v', x[o], y[o], z[o], collapse='\n')
  f.chr <- paste('f', t.ids[1,], t.ids[2,], t.ids[3,], collapse='\n')
  paste0(c(v.chr, f.chr), collapse='\n')
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
tol <- diff(range(map)) / 3
tris3 <- extract_mesh2(errors, tol)
# plot_tri_ids(tris, dim(errors))

# rayrender a mesh in list format

mesh.tri <- mx_to_mesh(vsq)
mesh.tri.s <- scale_mesh(mesh.tri)
mesh.obj <- mesh_to_obj(mesh.tri.s)

mesh.obj.2 <- tris_to_obj(tris, map)
writeLines(mesh.obj.2, f2)
mesh.obj.3 <- tris_to_obj(tris3, map)
writeLines(mesh.obj.3, f3)

library(rayrender)

writeLines(mesh.obj, f)

scn <- sphere(
  y=4, z = 2, x = 0, radius = 1,
  material = lambertian(lightintensity = 25, implicit_sample = TRUE)
)
scn <- add_object(
  scn,
  obj_model(
    filename=f2, x=.5, y=0, z=0.5, angle=c(90, 90, 0),
    material=lambertian(color='grey50')
  )
)
scn <- add_object(
  scn,
  obj_model(
    filename=f, x=-.75, y=0, z=0.5, angle=c(90, 90, 0),
    material=lambertian(color='grey50')
  )
)
scn <- add_object(
  scn,
  obj_model(
    filename=f3, x=1.75, y=0, z=0.5, angle=c(90, 90, 0),
    material=lambertian(color='grey50')
  )
)
scn <- add_object(
  scn, xz_rect(xwidth=5, zwidth=2, material=lambertian(color='grey50'))
)
render_scene(
  scn, 
  # width=400, height=400, samples=400,
  width=600, height=200, samples=200,
  lookfrom=c(0, 4, 1),
  lookat=c(0, 0.25, 0),
  aperture=0
)

library(rgl)
par3d(windowRect=c(20,20,400,400))
mesh.obj.2 <- tris_to_obj(tris, map)
writeLines(mesh.obj.2, f2)
plot3d(readOBJ(f2), col='grey50')
