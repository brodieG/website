source('static/script/_lib/rayrender.R')
source('static/script/_lib/plot.R')

library(rayrender)
library(ambient)    # for water patterns

eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))
der <- elmat1[-1,]
sea <- 50                 # sea level

# To actually extract meshes with variable tolerances
source('static/post/2020-01-11-mesh-red-vec_files/script/mesh-build.R')

# - Depth ----------------------------------------------------------------------

# For each depth = 0 point we want to find distane to the nearest ground point
# to use that as depth.  We're going to do brute force that by breaking up the
# map into smaller pieces.  This is not a general solution but will do for us.
# Could generate 75e6 nrow matrices with the current problem size (derwent)

# Take 2, find all the edges by looking at the 4 squares around each land square
# and see if there is any water adjacent

# stop()
# land <- der > sea
# idx <- as.matrix(subset(expand.grid(x=-1:1, y=-1:1), xor(x, y)))
# landi <- which(land, arr.ind=TRUE)
# idx2 <- landi[rep(seq_len(nrow(landi)), each=nrow(idx)), ] +
#   do.call(rbind, replicate(nrow(landi), idx, simplify=FALSE))
#
# # Use NAs to track oob, we'll make them land later
# idx2[idx2 == 0L] <- NA
# idx2[idx2[,1] > nrow(land), 1] <- NA
# idx2[idx2[,2] > ncol(land), 2] <- NA
# land.adj <- matrix(land[idx2], nrow(idx))
# land.adj[is.na(land.adj)] <- TRUE
#
# # Compute which lands are adjacent to water
# land.adj.water <- which(colSums(land.adj) < nrow(idx))
#
# # Too many, so sample randomly a subset that should be good enough
# set.seed(123)
# land.adj.w2 <- sample(land.adj.water, length(land.adj.water) / 2)
#
# lc <- t(landi[land.adj.w2,])
# wc <- t(which(!land, arr.ind=TRUE))
# wlcc <- expand.grid(w=seq_len(ncol(wc)), l=seq_len(ncol(lc)))
# wldist <- colSums((wc[, wlcc[['w']]] - lc[, wlcc[['l']]]) ^ 2)
# dim(wldist) <- c(ncol(wc), ncol(lc))
# nearl <- max.col(-wldist, ties.method='first')
# depth <- array(NA_real_, dim=dim(der))
# depth.dat <- sqrt(colSums((wc - lc[,nearl]) ^ 2))
# depth[t(wc)] <- (depth.dat - min(depth.dat)) / diff(range(depth.dat))
# saveRDS(depth, '~/Downloads/derwent/depth2.RDS')

der2 <- der - sea
depth2 <- readRDS('static/post/2020-01-11-mesh-red-vec_files/data/depth2.RDS')
depth.vals <- sqrt(depth2[!is.na(depth2)]) * (max(der2)) / 2

steps <- 360
stopifnot(!steps %% 4)
step.half <- steps / 2 + 1
step.qrt <- (step.half - 1) / 2 + 1
in.out.l <- c(
  seq(0, 1, length.out=step.half)[-step.half],
  seq(1, 0, length.out=step.half)[-step.half]
)

# - Mesh -----------------------------------------------------------------------

library(rtini)

# Force the water boundary to break to smallest tris
tol.cheat <- 200
der2[!is.na(depth2)] <- -depth.vals - (tol.cheat * 1.1)
err <- rtini_error(der2)
# plot(as.raster((der2 - min(der2))/diff(range(der2)))

# Add back the tolerance, but then a little step so water boundary at sharper
# angle

der2[!is.na(depth2)] <- der2[!is.na(depth2)] + (tol.cheat * 1.1) - 5

if(!exists('meshes') || 1) {
  # Pre-extract meshes and store them as we're likely to have many duplicate ones
  mesh.dir <- tempfile()  # remember to unlink this
  dir.create(mesh.dir)

  # We wish to build an approximate model of tolerance to number of polygons
  # produced

  writeLines('Estimating polys')
  err.approx <- sort(unique(as.integer(err[is.finite(err)] * 100)/100))
  err.breaks <- vapply(err.approx, function(x) sum(err > x), 0)
  interesting <- log(
    abs(diff(err.breaks)[-1]/diff(err.breaks)[-length(err.breaks) + 1])
  ) > 5
  tol.max <- 600
  tols.test <- rev(c(0, err.approx[which(interesting)], tol.max))
  polys <- vapply(
    tols.test,
    function(tol) {
      ids <- rtini_extract(err, tol=tol)
      sum(vapply(ids, ncol, 0))
    },
    0
  )
  # Want to spend a little extra time at top

  top <- .1
  top.steps <- floor(top / 2 * steps)
  steps.front <- c(
    seq(1, 0, length.out=steps / 2 - top.steps),
    rep(0, top.steps * 2 + 1)
  )
  steps.back <- seq(0, 1, length.out=steps / 2 - top.steps)
  in.out.tol <- c(steps.front, steps.back[-length(steps.back)])

  polys.tar <- 2^(
    (in.out.tol) *
    diff(log2(range(polys))) + min(log2(polys))
  )
  # Tolerances that correspond to polygon counts
  tols <- tols.test[findInterval(polys.tar + 1, polys, left.open=TRUE)]

  # Find tolerances that lead to different error counts so we don't compute
  # duplicates.
  tol.breaks <- vapply(tols, function(x) sum(err > x), 0)
  tol.break.w <- match(tol.breaks, tol.breaks)
  tol.break.wu <- unique(tol.break.w)

  for(i in seq_along(tol.break.wu)) {
    mesh.f <- sprintf("mesh-%04d.obj", i)
    cat(sprintf("\rbuilding mesh %s (%s)", mesh.f, as.character(Sys.time())))
    build_der_mesh(
      der2, err, tols[tol.break.wu[i]], file=file.path(mesh.dir, mesh.f)
    )
  }
  meshes <- list.files(mesh.dir, full.names=TRUE)
  cat("\n")
}

# - Scene & Render -------------------------------------------------------------

xang <- 80
xw <- diff(range(xyz$x)) * .999
zw <- .999
ymin <- min(unlist(mesh[, 'z']))
cxy <- expand.grid(x=seq_len(nrow(der)),y=seq_len(ncol(der)))

# Camera Path, will be decomposed in move-in-out of starting point along with
# rotation of the object.

la0 <- numeric(3)
la1 <- c(0, 0, 0.05)

# lf0 <- c(.15, 0.01, .3)
lf0 <- c(0, .01, .3)
lf1 <- c(0, 0.5, 0.9) * 4

in.out.a <- ease_in_smooth_out(step.half, function(x) x ^ 5, c(.75,.75))$y
in.out <- c(in.out.a[-length(in.out.a)], rev(in.out.a[-1]))
around <- c(in.out.a[-length(in.out.a)], -rev(in.out.a[-1]) + 2)

# Camera effective distance, the ratio of distance * fov, is a bit tricky
# because we have specific effective distances in mind, but also want
# constraints on what the initial and final distance and FOV values are (at
# least FOV), and we want the two to change indepedantly while their product
# changes monotonically.

efd.s <- 4
efd.e <- 14

# fov: (m*x + a)
# dist: (n*x + b)
# Subject to x in [0,1]
# effdist: (m*x + a) * (n*x + b)
# m * n = -3
# a * b = efd.s                          | x = 0
# m * n + a * n + b * m + efd.s = efd.e  | x = 1

# Fix some of the degrees of freedom.
b <- 1/2
a <- efd.s / b
mn <- -3        # `n` unknown, but we fix m * n to mn

# Solve using quadratic equation.  Above reformulates to:
#   a * n^2 - (efd.e - efd.s - mn) * n + b * mn = 0
# and need to solve for n (note `mn` is a constant)
A <- a
B <- -(efd.e - efd.s - mn)
C <- b * mn
n <- (-B + sqrt(B^2 - 4 * A * C)) / (2 * A)
m <- mn / n

# Now need to convert magnification factors into actual fov and distance
# multipliers from the base distance.  Let's fix the starting and ending fovs
# and let the rest fall out ouf it

fovm0 <- a
fovm1 <- m + a
fov0 <- 80
fov1 <- fov0 / fovm0 * fovm1

distm0 <- b
distm1 <- n + b
dist0 <- sqrt(sum((lf0 - la0)^2))
dist1 <- dist0 / distm0 * distm1

lav <- la1 - la0
lfv <- lf1 - lf0
la <- la0 + matrix(lav) %*% t(in.out)
lf.raw <- lf0 + matrix(lfv) %*% t(in.out)

# Adjust distances to match overall desired zoom levels spec'ed in efd.s and
# efd.e (effective distance)
ld <- sqrt(colSums((lf.raw - la) ^ 2))
lf <- la + (lf.raw - la) / rep(ld, each=3) *
  rep((((ld - dist0) * (dist1 - dist0) / max(ld - dist0)) + dist0), each=3)
dist <- sqrt(colSums((lf - la) ^ 2))
dist.r <- dist / dist[1]

fovs <- fov0 - (fov0 - fov1) * in.out

angs.base <- -(cos(seq(0, pi, length.out=steps)) - 1) * .5
angs.base <- cumsum(c(0, diff(angs.base)^2))
angs <- angs.base / max(angs.base) * 360

step.i <- matrix(seq_len(steps), 8, byrow=TRUE)

for(i in c(step.i)) {
  j <- i
  ti <- i

  ang <- angs[i]
  # angle to use for wave pattern, offset to an angle were we won't notice the
  # repeat happening (i.e. when looking far from water / behind mountains).
  angc <- (((i - 1) / steps) * 360 + 202) %% 360  # constant speed angle
  writeLines(sprintf('Frame %d %s', i, Sys.time()))

  # Water, adapted from:
  # https://gist.github.com/tylermorganwall/7f31a10f22dc5912cc86b8b312f6f335
  fbase <- .025
  amb <- gen_simplex(cxy[,1], cxy[,2], z=angc, frequency = fbase, seed = 1) +
    gen_simplex(cxy[,1], cxy[,2], z=angc, frequency = fbase * 2, seed = 2) / 2 +
    gen_simplex(cxy[,1], cxy[,2], z=angc, frequency = fbase * 4, seed = 3) / 4
  amb <- (amb - min(amb)) / diff(range(amb))
  att <- c(1,.7,0.3) * 4
  ref <- 1.33
  water <- dielectric( # color='#E0F5FF',
    refraction=ref,
    bump_texture=matrix(amb, nrow(der), ncol(der)),
    bump_intensity=3,
    attenuation=att
  )
  water2 <- dielectric(refraction=ref, attenuation = att)
  water <- water2 <- diffuse(color='#E0F5FF')

  water.obj <- group_objects(
    dplyr::bind_rows(
      xz_rect(material=water,  xwidth=xw, y=0, zwidth=zw),
      xz_rect(material=water2, xwidth=xw, y=ymin, flipped=TRUE, zwidth=zw),
      xy_rect(
        material=water2, xwidth=xw, y=ymin/2, z=-zw/2, ywidth=-ymin, flipped=TRUE
      ),
      xy_rect(material=water2, xwidth=xw, y=ymin/2, z=zw/2,  ywidth=-ymin),
      yz_rect(
        material=water2, y=ymin/2, x=xw/2,  ywidth=-ymin, zwidth=zw,
        flipped=TRUE
      ),
      yz_rect(material=water2, y=ymin/2, x=-xw/2, ywidth=-ymin, zwidth=zw),
    ),
    pivot_point=numeric(3),
    group_angle=c(0, ang, 0)
  )
  # Assemble scene
  scene <- dplyr::bind_rows(
    sphere(y=5, z=3, x=2, radius=1, material=light(intensity=30)),
    group_objects(
      obj_model(
        meshes[match(tol.break.w[ti], tol.break.wu)], vertex_colors=TRUE
      ),
      # obj_model(meshes[length(meshes)], vertex_colors=TRUE),
      pivot_point=numeric(3), group_angle=c(90, ang, 180),
      group_order_rotation=c(3, 1, 2),
    ),
    water.obj,
    # "sky" reflector
    xz_rect(
      xwidth=6, zwidth=6, y=6,
      material=diffuse('deepskyblue'), flipped=TRUE,
      angle=c(25, 0, 0)
    ),
  )
  render_preview(
    scene,
    fov=fovs[i],
    # width=800, height=800, samples=100,
    # width=1200, height=1200, samples=400,
    width=600, height=600, samples=10,
    # width=400, height=400, samples=25,
    lookat=la[,i],
    lookfrom=lf[,i],
    aperture=0,
    clamp_value=5,
    # debug_channel='normals',
    # filename=next_file('~/Downloads/derwent/vmov-5/img-001.png')
    filename=sprintf('~/Downloads/derwent/vmov-5/img-%3d.png', i)
    # filename=next_file('~/Downloads/derwent/v1/img-000.png')
  )
}
# unlink(mesh.dir, recursive=TRUE)
stop('done render')

# - Perlin Experiments ---------------------------------------------------------

steps <- 360
angs <- seq(0, 360, length.out=steps + 1)
hzs <- c(.2, .1, .05, .025)
for(j in seq_len(steps)) {
  png(next_file('~/Downloads/derwent/perlin3/img-000.png'))
  par(mai=numeric(4))
  i <- ang <- angs[j]
  cxy <- expand.grid(x=seq_len(nrow(der)),y=seq_len(ncol(der)))
  # fbase <- 0.05
  # fbase <- hzs[j]
  fbase <- .025
  amb <- gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase, seed = 1) +
    gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase * 2, seed = 2)/2 +
    gen_simplex(cxy[,1],cxy[,2],z=i, frequency = fbase * 4, seed = 3)/4 +
    0

  amb <- (amb - min(amb)) / diff(range(amb))
  plot(as.raster(array(amb, dim=dim(der))))
  dev.off()
}
stop()
x <- list.files('~/Downloads/derwent/perlin3', pattern='^img', full.names=TRUE)
file.copy(
  x[c(-1,-length(x))],
  file.path(
    dirname(x[[1]]),
    sprintf("img-%03d.png", rev(seq_len(length(x) - 2) + length(x)))
  )
)

file.copy(a, b)
