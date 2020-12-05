# - Base Colors ----------------------------------------------------------------

gold <- '#CCAC00'
metal.col <-  c(gold, 'grey75', '#CC3322')
mesh.colors <- metal.col

# - Misc Tools -----------------------------------------------------------------

rep_each <- function(x, each) {
  if(each) {
    res <- matrix(x, each, length(x), byrow=TRUE)
    dim(res) <- NULL
    res
  } else x[0]
}

# - Plot Helper Tools ----------------------------------------------------------

## Rescale data to a range from 0 to `range` where `range` in (0,1]
rescale <- function(x, range=1, center=0.5)
  if(range <= 0) x else
  ((x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))) * range +
   (1 - range) * center

## Prepare a plot with a particular aspect ratio
plot_new <- function(
  x, y, xlim=c(0,1), ylim=c(0,1),
  mai=numeric(4L), xaxt='n', yaxt='n', xaxs='i', yaxs='i', ...
) {
  args <- as.list(environment())
  do.call(par, args[!names(args) %in% c('x', 'y', 'xlim', 'ylim')])
  plot.new()
  plot.window(
    xlim, ylim, asp=diff(range(y, na.rm=TRUE))/diff(range(x, na.rm=TRUE))
) }
## @param ... parameters based on to `par` call when `new=TRUE`

plot_tri_ids <- function(tri, dim) {
  ids <- rbind(matrix(unlist(tri), 3L), matrix(unlist(tri), 3L)[1,], NA) - 1L
  x <- ids %/% dim[1]
  y <- ids %% dim[1]
  plot_tri_xy(x, y, dim, new=new, lwd=lwd, col=col, ...)
}
plot_tri_xy <- function(x, y, dim, new=TRUE, lwd=1, col='#444444', ...) {
  x <- x - 1
  y <- y - 1
  if(new) plot_new(x, y, ...)
  # polygon(x/(dim[2] - 1), y/(dim[1]-1), col='#DDDDDD', border='#444444')
  lines(x/(dim[2] - 1), y/(dim[1]-1), col=col, lwd=lwd)
}

plot_points_ids <- function(points.ids, dim, cex=1, col='red') {
  ids <- points.ids - 1L
  x <- ids %/% dim[1]
  y <- ids %% dim[1]
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
    xs <- matrix(raw %/% nrow(errors), 3)
    ys <- matrix(raw %% ncol(errors), 3)

    plot_new(xs, ys)
    polygon(
      rescale(rbind(xs, NA)), rescale(rbind(ys, NA)),
      col='#DDDDDD', border='#444444'
    )
  }
}
# map <- matrix(c(0, 0, 0, 0, 1, 0, 0, 1, 0), nrow=3)

# Mx to data frame, scale of 0 doesn't rescale,

mx_to_df <- function(mx, scale=rep(1, 3)) {
  df <- reshape2::melt(mx)
  names(df) <- c('y', 'x', 'z')
  df <- df[,c('x', 'y', 'z')]
  if(length(scale)) df[] <- Map(rescale, df, scale, numeric(3))
  df
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
# Convert list-matrix mesh into obj text format, relying on list matrix
# being in counter-clockwise direction outwards

mesh_to_obj <- function(mesh, center=NULL) {
  # first step is convert to array, drop texture for now

  mesh.arr <- array(unlist(mesh[1:3, 1:3]), c(length(mesh[[1]]), 3, 3))

  if(!is.null(center)) {
    # we need to re-order the faces so their normals all point away from the
    # center

    a <- mesh.arr[,2,] - mesh.arr[,1,]
    b <- mesh.arr[,3,] - mesh.arr[,1,]

    norm <- rbind(
      a[,2] * b[,3] - a[,3] * b[,2],
      a[,3] * b[,3] - a[,1] * b[,3],
      a[,1] * b[,2] - a[,2] * b[,1]
    )
    xc <- center - t(rowMeans(aperm(mesh.arr, c(1,3,2)), dim=2))
    flip <- colSums(xc * norm) > 0

    mesh.arr[flip,2:3,] <- mesh.arr[flip,3:2,]
  }
  mesh.v <- matrix(aperm(mesh.arr, c(3, 2, 1)), nrow=3)
  v.chr <- paste('v', mesh.v[1,], mesh.v[2,], mesh.v[3,], collapse="\n")

  # faces are easy because we've repeated the vertices so no shared vertices

  mesh.f <-matrix(seq_len(length(mesh[[1]]) * 3), nrow=3)
  f.chr <- paste('f', mesh.f[1,], mesh.f[2,], mesh.f[3,], collapse="\n")

  paste0(c(v.chr, f.chr), collapase="\n")
}
# Return format from extract_mesh2, the vertices are returned in triangle order
# (i.e. first three are first triangle, next 3 are second triangle, etc.)
#
# col -> x
# row -> y
#
# Note the above used to be backwards.  Now this way the matrix and the plots
# are in the same order, though y values still go in opposite direction.
#
# We need `map` to get row/col counts, but also to have access to the full
# height map so whe nwe scale we scale relative to the full heightmap, not the
# portion of it captured by `tris`.

ids_to_xyz <- function(tris, map, scale, flatten=FALSE) {
  ids <- unlist(tris)
  if(!length(ids)) {
    # minimal triangle
    nr <- nrow(map)
    tr <- length(map) - nrow(map) + 1L
    ids <- c(1L, nr, length(map), 1L, tr, length(map))
  }
  x <- (ids - 1) %/% dim(map)[1]
  y <- (ids - 1) %% dim(map)[1]
  z <- map[c(ids)]

  x <- x / (dim(map)[2] - 1) * scale[1]
  y <- y / (dim(map)[1] - 1) * scale[2]
  z <- if(flatten) numeric(length(z))
       else (z - min(map)) / (diff(range(map))) * scale[3]
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
xyz_to_mesh <- function(xyz) {
  xyzm <- lapply(xyz, matrix, 3)
  xyzv1 <- lapply(xyzm, '[', 1,)
  xyzv2 <- lapply(xyzm, '[', 2,)
  xyzv3 <- lapply(xyzm, '[', 3,)
  t(
    matrix(
      c(xyzv1, xyzv2, xyzv3),
      c(3, 3),
      dimnames=list(c('x','y','z'), paste0('v', 1:3))
  ) )
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
# Generate sphere joints at the vertices of the triangles

tris_to_joints <- function(tris, map, material, radius) {
  coords <- unique(tris_to_df(tris, map))
  dplyr::bind_rows(
    lapply(
      seq_len(nrow(coords)),
      function(i) {
        sphere(
          coords[i,'x'], coords[i,'y'], coords[i,'z'],
          material=material,
          radius=radius
        )
      }
) ) }


# We call shards a two triangle sandwich with the faces closed off by bevels,
# which would look kind of like an arrowhead.
#
# Given x-y-z coordinates of triangles, a depth, and bevel angle, return
# the corresponding coordinates of the shard / arrow head.
#
# x-y-z coordinates are ordered by triangle (i.e. first 3 of each are first
# triangle, and so on).  The vertex order within each triangle is considered
# important and will be used to derive the vertex orders for the new faces.
#
# This is not particularly efficient as we are not trying to minimize data
# copies in our array re-arrangements, but it seems unlikely that will be the
# bottleneck.

xyz_to_shard <- function(xyz, depth, bevel, flatten=FALSE) {
  vetr(
    list(x=numeric(), y=numeric(), z=numeric()) && length(unique(lengths(.))) == 1,
    NUM.1.POS, NUM.1.POS && . <= 90
  )
  # this is inefficient...
  x <- matrix(xyz[['x']], 3)
  y <- matrix(xyz[['y']], 3)
  z <- matrix(xyz[['z']], 3)
  if(flatten) z[] <- 0
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
  cos.ab <- (rowSums(V * W) / (sqrt(rowSums(V ^ 2)) * sqrt(rowSums(W ^ 2))))
  delta <- bl / cos(acos(cos.ab) / 2)

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

  # We have all the vertices for each triangle we will organize them into
  # an array so that we can then index them to define the faces.

  # NOTE BOTTOM FACE NEEDS TO BE FLIPPED, maybe we can just compute normals
  # relative to barycenter and make sure they are all flipped correctly

  vall <- array(
    c(v1t, v2t, v3t, v1, v2, v3, v1b, v2b, v3b),
    c(dim(v1t), 3, 3),
    dimnames=list(NULL, c('x','y','z'), paste0('v',1:3), c('top','mid','bot'))
  )
  vid <- array(
    seq_len(length(vall)/3), dim(vall)[c(1,3,4)],
    dimnames=dimnames(vall)[c(1,3,4)]
  )
  fids <- rbind(
    # bevel faces
    t(
      matrix(
        unlist(
          lapply(
            list(1:2, 2:3, c(3,1)),
            function(x) {
              c(
                vid[,x[1],'top'], vid[,x[2],'top'], vid[,x[2],'mid'],
                vid[,x[2],'mid'], vid[,x[1],'mid'], vid[,x[1],'top'],
                vid[,x[1],'mid'], vid[,x[2],'mid'], vid[,x[2],'bot'],
                vid[,x[2],'bot'], vid[,x[1],'bot'], vid[,x[1],'mid']
              )
        } ) ),
        nrow=dim(vid)[1]
    ) ),
    # top and bottom faces

    t(
      matrix(
        c(vid[,,'top'], vid[,c(1,3,2),'bot']),  # reorder bot vert for normals
        nrow=dim(vid)[1]
    ) )
  )
  dim(fids) <- c(3, length(fids) / 3)

  # drop the top/bot/mid distinction in the original vertex list, and move
  # x/y/z dim to be last so we have per-vertex order

  vfin <- matrix(aperm(vall, c(1, 3, 4, 2)), ncol=3)

  # For each face, compute the angle between the normal and the weighted center
  # of the vertices

  list(vertices=vfin, faces=fids)
}
shard_to_obj <- function(shard) {
  vetr(
    list(
      vertices=matrix(numeric(), ncol=3),
      faces=matrix(numeric(), nrow=3)
    )
  )
  v <- shard[['vertices']]
  f <- shard[['faces']]
  v.chr <- paste('v', v[,1], v[,2], v[,3], collapse="\n")
  f.chr <- paste('f', f[1,], f[2,], f[3,], collapse="\n")
  c(v.chr, f.chr)
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
xyz_to_cube <- function(xyz, material, xwidth, zwidth, angle, translate) {
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
          cubement(
            start=segdat[i, 1:3], end=segdat[i, 4:6],
            xwidth=xwidth, zwidth=zwidth, material=material
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
tris_to_cube <- function(
  tris, map, scale=c(1, 1, 1), material=diffuse(), xwidth=1, zwidth=1,
  angle=c(0,0,0), translate=c(0,0,0), flatten=FALSE
) {
  xyz_to_cube(
    ids_to_xyz(tris, map, scale, flatten), 
    material, xwidth, zwidth, angle, translate
  )
}

mesh_to_seg <- function(
  mesh, map, scale=c(1, 1, 1), material=diffuse(), radius=1,
  angle=c(0,0,0), translate=c(0,0,0)
) {
  xyz_to_seg(mesh_to_xyz(mesh, map, scale), material, radius, angle, translate)
}
# Adaptation of rayender::segment

cubement <- function (start = c(0, -1, 0), end = c(0, 1, 0), 
  xwidth = 1, zwidth = 1,
    material = diffuse(), velocity = c(0,
        0, 0), flipped = FALSE, scale = c(1, 1, 1))
{
  if (length(scale) == 1) {
    scale = c(scale, scale, scale)
  }
  x = (start[1] + end[1])/2
  y = (start[2] + end[2])/2
  z = (start[3] + end[3])/2
  order_rotation = c(3, 2, 1)
  phi = atan2(end[1] - start[1], end[3] - start[3])/pi * 180 + 90
  length_xy = sqrt((end[1] - start[1])^2 + (end[3] - start[3])^2)
  if (end[1] == start[1] && end[3] == start[3]) {
      theta = 0
  }
  else {
      theta = atan2(-length_xy, (end[2] - start[2]))/pi * 180
  }
  fulllength = sqrt(sum((end - start)^2))
  angle = c(0, phi, theta)
  boxinfo = c(unlist(material$properties), xwidth, fulllength,
        zwidth)

  tibble::tibble(x = x, y = y, z = z, radius = NA, type = material$type,
    shape = "box", properties = list(boxinfo), velocity = list(velocity),
    checkercolor = material$checkercolor, noise = material$noise,
    noisephase = material$noisephase, noiseintensity = material$noiseintensity,
    noisecolor = material$noisecolor, angle = list(angle),
    image = material$image, lightintensity = material$lightintensity,
    flipped = flipped, fog = material$fog, fogdensity = material$fogdensity,
    implicit_sample = material$implicit_sample, sigma = material$sigma,
    order_rotation = list(order_rotation), pivot_point = list(NA),
    group_translate = list(NA), group_angle = list(NA),
    group_order_rotation = list(NA),
    tricolorinfo = list(NA), fileinfo = NA, scale_factor = list(scale),
    group_scale = list(NA)
  )
}

# Render a list of scenes into files

render_scenes <- function(scene, filename='scene-%d.png', ...) {
  lapply(
    seq_along(scene),
    function(i) {
      writeLines(sprintf("Starting frame %d at %s", i, as.character(Sys.time())))
      render_scene(scene=scene[[i]], filename=sprintf(filename, i), ...)
} ) }
# Convert triangles to x/y coordinates with colors
#
# Intended specifically for illustrating the tiling of the triangles

tris_to_df <- function(tris, map) {
  tris <- as.data.frame(ids_to_xyz(tris, map, c(1,1,1)))
  tris[['id']] <- rep(seq_len(nrow(tris) / 3), each=3)

  # By design the triangle vertices are ordered so the first two vertices define
  # the hypotenuse.  Find direction of hypotenuse to opposite vector

  xs <- matrix(tris[['x']], 3)
  ys <- matrix(tris[['y']], 3)
  xh <- (xs[2,] + xs[1,]) / 2
  yh <- (ys[2,] + ys[1,]) / 2
  xd <- (xs[3,] - xh)
  yd <- (ys[3,] - yh)

  # vector angle relative to (0, 1) vector, calculation simplified as x == 0

  angle <- round(acos(yd / sqrt(colSums(rbind(xd, yd)^2))) / pi * 180) *
    ifelse(xd, sign(xd), 1)
  base.angles <- seq(-180+45, 180, by=45)

  tris[['shape']] <- rep(match(angle, base.angles), each=3)
  # thank you Brewer
  colors <- c(
    '#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0',
    '#225ea8','#0c2c84'
  )
  tris[['color']] <- colors[c(1:4 * 2 - 1, 1:4 * 2)][tris[['shape']]]
  tris
}
# Convert matrix id coords to x/y coords
#
# Intended specifically for illustrating the tiling of the triangles

ids_to_df <- function(ids, map) {
  as.data.frame(ids_to_xyz(ids, map, c(1,1,1)))
}
# - PNG stuff ------------------------------------------------------------------

# Read in pngs, and write them back stitched together side by side

cbind_pngs <- function(input, output) {
  vetr(character(), character(1L))
  pngs <- lapply(input, png::readPNG)
  png.dims <- vapply(pngs, dim, numeric(3))
  if(length(unique(png.dims[1,])) != 1) stop("different row counts on pngs")
  if(length(unique(png.dims[3,])) != 1) stop("different chanel counts on pngs")

  cols <- sum(png.dims[2,])
  colc <- cumsum(png.dims[2,])
  colcs <- 1L + c(0L, head(colc, 2L))
  d <- array(numeric(), c(png.dims[1,1],sum(png.dims[2,]),png.dims[3,1]))

  for(i in seq_along(colc)) d[,colcs[i]:colc[i],] <- pngs[[i]]
  png::writePNG(d, output)
}
# trim white or near white rows/col (for raster image rows are the x
# coordinatese of the arrays).  We don't bother with alpha as that will require
# us cheking if it has the channel and different handling for it.
#
# @param pngs a list of png arrays (3D), with or without alpha
# @param tol numeric(2), tolerance for rows and cols in that order
# @param pad numeric(2), pixels of padding to add to result will just be full
#   opacity white
# @return list of png arrays

trim_png <- function(pngs, tol=c(2,2), pad=c(0,0)) {
  res <- trim_png_row(pngs, tol[1], pad[1])
  trim_png_col(res, tol[2], pad[2])
}

trim_png_internal <- function(pngs, tol=2, pad=0, which) {
  vetr(
    list() && length(.) > 0, INT.1 && all_bw(., 0, 255), INT.1.POS,
    which=isTRUE(. %in% c('row', 'col'))
  )
  if(which == 'row') {
    sumf <- rowSums
    readf <- function(x, idx) x[idx,,]
    writef <- function(x, idx, val) {
      x[idx,,] <- val
      x
    }
    tar.dim <- 1
    dim.len <- dim(pngs[[1]])[1]
  } else {
    sumf <- colSums
    readf <- function(x, idx) x[,idx,]
    writef <- function(x, idx, val) {
      x[,idx,] <- val
      x
    }
    tar.dim <- 2
    dim.len <- dim(pngs[[1]])[2]
  }
  tol <- tol / 255
  dims <- sapply(pngs, dim)
  if(!is.matrix(dims)) stop("unequal dims")
  if(length(unique(dims[1,])) > 1 || length(unique(dims[2,])) > 1)
    stop("unequal dim2")

  pngsum <- rowSums(sapply(pngs, function(x) sumf(x < 1 - tol))) == 0
  a <- min(which(!pngsum))
  b <- min(which(!rev(pngsum)))

  a.not <- seq_len(a - 1L)
  b.not <- seq_len(b - 1L) + dim.len - b + 1

  tmp <- lapply(pngs, readf, -c(a.not, b.not))
  dim.tmp <- dim(tmp[[1]])
  if(pad) {
    dim.pad <- dim.tmp
    dim.pad[tar.dim] <- dim.pad[tar.dim] + 2*pad
    res.tpl <- array(1, dim.pad)
    lapply(tmp, writef, idx=seq_len(dim.tmp[tar.dim]) + pad, x=res.tpl)
  } else {
    tmp
  }
}
trim_png_row <- function(pngs, tol=2, pad=0) {
  trim_png_internal(pngs, tol, pad, which='row')
}
trim_png_col <- function(pngs, tol=2, pad=0) {
  trim_png_internal(pngs, tol, pad, which='col')
}



