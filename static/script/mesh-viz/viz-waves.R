# Generate a water surface
#
# @param f the file name to write the obj file to
# @param n how many elements to the side the surface should be
# @param t the time offset

water_surface <- function(f, n=50, t=0, hz=1) {
  seqv <- seq(-pi,pi, length.out=n + 1)
  df <- expand.grid(x=seqv, y=seqv)
  df <- transform(
    df, z=
      .5 * sin(2*(x+t)*hz + 4 * (y+t)*hz) +
      .125 * sin(2*(((x+t) + pi)*hz)^2 + 4*((y+t)*hz)^2) +
      .125 * sin(3*(((x+t) + pi/4)*hz)^2 + 2*(((y+t) + pi)*hz)^2) +
      .3 * sin(2*(x+t)*hz - 3 * (y+t)*hz)
    )
  # library(ggplot2)
  # ggplot(df, aes(x=x, y=-y, fill=z)) + geom_raster() + scale_fill_viridis_c()

  # f <- tempfile()
  mesh <- shadow::mesh_tri(transform(df, y=z*.05, z=y), c(n, n)+1)
  m1 <- mesh
  m1[] <- lapply(mesh, '[', 1)

  # Need to find all the vertices on the edges and generate a border out of them

  xu <- unlist(mesh[,'x'])
  zu <- unlist(mesh[,'z'])
  yu <- unlist(mesh[,'y'])

  x.lo <- min(xu)
  x.hi <- max(xu)
  z.lo <- min(zu)
  z.hi <- max(zu)
  y.lo <- -10
  if(y.lo > min(yu)) stop('need lower y')

  vertex_arrange <-function(a, b, y, lo) {
    which.a <- b == lo
    base.a <- a[which.a]
    o <- order(base.a)
    ao <- base.a[o]
    d <- duplicated(ao)
    aou <- ao[!d]
    you <- y[which.a][o][!d]
    list(aou, you)
  }
  zx.lo <- vertex_arrange(zu, xu, yu, x.lo)
  zx.hi <- vertex_arrange(zu, xu, yu, x.hi)
  xz.lo <- vertex_arrange(xu, zu, yu, z.lo)
  xz.hi <- vertex_arrange(xu, zu, yu, z.hi)

  to_mesh <- function(v, w, y, y.lo) {
    lv <- length(v)
    y.lo.r <- rep(y.lo, lv - 1)

    L <- matrix(
      list(
        c(v[-lv], v[-1]), c(v[-lv], v[-1]), c(v[-1], v[-lv]),
        c(y[-lv], y.lo.r), c(y.lo.r, y[-1]),  c(y[-1], y.lo.r),
        rep(w, (lv - 1) * 2), rep(w, (lv - 1) * 2), rep(w, (lv - 1) * 2)
      ),
      3
    )
  }
  mzx.lo <- to_mesh(zx.lo[[1]], x.lo, zx.lo[[2]], y.lo)[,3:1]
  mzx.hi <- to_mesh(zx.hi[[1]], x.hi, zx.hi[[2]], y.lo)[,3:1]
  mxz.lo <- to_mesh(xz.lo[[1]], z.lo, xz.lo[[2]], y.lo)
  mxz.hi <- to_mesh(xz.hi[[1]], z.hi, xz.hi[[2]], y.lo)
  # bot <- matrix(
  #   list(
  #     c(x.lo, x.lo), c(x.hi, x.hi), c(x.hi, x.lo),
  #     c(y.lo, y.lo), c(y.lo, y.lo), c(y.lo, y.lo),
  #     c(z.lo, z.lo), c(z.lo, z.hi), c(z.hi, z.hi)
  #   ), 3
  # )
  mesh2 <- mesh[,1:3]
  # mesh2[] <- Map(c, mesh2, mzx.lo, mzx.hi, mxz.lo, mxz.hi, bot)
  mesh2[] <- Map(c, mesh2, mzx.lo, mzx.hi, mxz.lo, mxz.hi)
  mesh3 <- mesh2

  # obj <- mesh_to_obj(mesh3, c(0, y.lo * .98, 0))
  obj <- mesh_to_obj(mesh3, c(0, -5, 0))
  # obj <- mesh_to_obj(mesh3)
  writeLines(obj, f)
  f
}

