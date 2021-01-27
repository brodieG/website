# source('content/post/2021-01-23-out-of-r-depth/script/crater.R')
library(vetr)
library(viridisLite)
library(rayrender)
library(ambient)    # for water patterns
source('static/script/_lib/rayrender.R')
source('static/script/_lib/plot.R')
source('content/post/2021-01-23-out-of-r-depth/script/plot.R')

# http://oe.oregonexplorer.info/craterlake/dem.html
eltif <- raster::raster("~/Downloads/dems_10m.dem")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
# not memory efficient but oh well, can't be bothered learning
# how to do this properly
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

map <- elmat1[1500:2599 + 150, 1500:2599]

find_water <- function(map) {
  stopifnot(length(map) > 0)
  map.rle <- rle(sort(c(map)))
  water.level <- map.rle$values[which.max(map.rle$lengths)]
  # Asssume no water on edges of map
  water <- map == water.level
  water[c(1,nrow(water)),] <- FALSE
  water[,c(1,ncol(water))] <- FALSE
  water
}
find_shore <- function(water) {
  stopifnot(length(water) > 0)
  neighbors <- as.matrix(subset(expand.grid(x=-1:1, y=-1:1), x | y))
  water.i <- which(water, arr.ind=TRUE)
  shore <- array(FALSE, dim(water))
  for(i in seq_len(nrow(neighbors))) {
    shore[
      cbind(
        water.i[, 1] + neighbors[i, 1],
        water.i[, 2] + neighbors[i, 2]
      )
    ] <- TRUE
  }
  shore <- shore & !water
}
dist_brute <- function(map, water.i, shore.i) {
  water.j <- t(water.i)
  water.d <- rep(Inf, nrow(water.i))
  for(i in seq_len(nrow(shore.i))) {
    dist <- colSums((water.j - shore.i[i,]) ^ 2)
    nearer <- dist < water.d
    water.d[nearer] <- dist[nearer]
  }
  dist <- array(0, dim(map))
  dist[water.i] <- sqrt(water.d)
  dist
}
dist_xy <- function(x, y) (x * x) + (y * y)
dist_near <- function(x, y) x < y
sub_val <- function(x, y) x - y

dist_brute2 <- function(map, water.i, shore.i) {
  wx <- as.numeric(water.i[,1])
  wy <- as.numeric(water.i[,2])
  sx <- as.numeric(shore.i[,1])
  sy <- as.numeric(shore.i[,2])
  water.d <- rep(Inf, nrow(water.i))
  for(i in seq_along(sx)) {
    dist <- ((wx - sx[i]) ^ 2 + (wy - sy[i]) ^ 2)
    nearer <- which(dist < water.d)
    water.d[nearer] <- dist[nearer]
  }
  dist <- array(0, dim(map))
  dist[water.i] <- sqrt(water.d)
  dist
}
dist_brute3 <- function(map, water.i, shore.i) {
  wx <- as.numeric(water.i[,1])
  wy <- as.numeric(water.i[,2])
  sx <- as.numeric(shore.i[,1])
  sy <- as.numeric(shore.i[,2])
  dist <- array(0, dim(map))
  for(i in seq_along(wx)) {
    xi <- wx[i]
    yi <- wy[i]
    dist[xi, yi] <- min(((sx - xi) ^ 2 + (sy - yi) ^ 2))
  }
  sqrt(dist)
}
# Deduplicate two col matrices
#
# ~10x faster than duplicated.

dedupi <- function(x) {
  if(nrow(x)) {
    len <- nrow(x)
    o <- order(x[,1], x[,2])
    x <- x[o,, drop=FALSE]
    tmp <- abs(x[-1,,drop=FALSE] - x[-len,,drop=FALSE])
    c(TRUE, tmp[,1] | tmp[,2])[order(o)]
  } else logical()
}
# this could be made faster as we don't need the double order
# (i.e. don't use dedupi which has to re-order back)
dedup <- function(x) x[dedupi(x),,drop=FALSE]

dist1 <- function(map, water.i, shore.i, degree=2) {
  m <- array(0, dim(map))
  m[water.i] <- Inf
  s <- shore.i
  d <- numeric(nrow(s))
  range <- (-degree):degree
  neigh <- as.matrix(subset(expand.grid(x=range, y=range), x | y))
  neigh <- cbind(neigh, d=sqrt(rowSums(neigh^2)))
  i <- 0
  while(nrow(s)) {
    # if(!(i <- i + 1) %% 10) writeLines(sprintf("%d rows: %d", i, nrow(s)))
    # writeLines(sprintf("%d rows: %d", i, nrow(s)))
    ss <- s[rep(seq_len(nrow(s)), each=nrow(neigh)),] +
      neigh[rep(seq_len(nrow(neigh)), nrow(s)), 1:2]
    inb <-
      ss[,1] > 0 & ss[,1] <= nrow(map) &
      ss[,2] > 0 & ss[,2] <= ncol(map)
    ss <- ss[inb,]
    d.new <- rep(d, each=nrow(neigh)) + neigh[,3]
    d.new <- d.new[inb]
    d.nearer <- which(d.new < m[ss])
    d.new.near <- d.new[d.nearer]
    d.new.o <- order(-d.new.near)
    d <- d.new.near[d.new.o]
    s <- ss[d.nearer[d.new.o],,drop=FALSE]
    m.old <- m
    m[s] <- d    # this dedupes
    s <- dedup(s)
    d <- m[s]
  }
  m
}
# This returns the distances squared
dist2 <- function(map, water.i, shore.i) {
  m <- array(0, dim(map))
  m[water.i] <- Inf
  s <- shore.i
  d <- matrix(0, nrow(s), 2)
  range <- (-1):1
  # neigh <- as.matrix(subset(expand.grid(x=range, y=range), xor(x,y)))
  neigh <- as.matrix(subset(expand.grid(x=range, y=range), x|y))
  i <- 0
  while(nrow(s)) {
    # writeLines(sprintf("%d rows: %d", i, nrow(s)))
    s.rep <- rep(seq_len(nrow(s)), each=nrow(neigh))
    n.rep <- rep(seq_len(nrow(neigh)), nrow(s))
    nn <- neigh[n.rep,]
    ss <- s[s.rep,] + nn
    inb <-
      ss[,1] > 0 & ss[,1] <= nrow(map) &
      ss[,2] > 0 & ss[,2] <= ncol(map)
    ss <- ss[inb,,drop=FALSE]
    d.new <- d[s.rep,] + nn
    d.new <- d.new[inb,,drop=FALSE]
    d.new.d <- rowSums(d.new ^ 2)
    d.nearer <- which(d.new.d < m[ss])
    d.new.o <- order(d.new.d[d.nearer])
    d.nearer.o <- d.nearer[d.new.o]
    d <- d.new[d.nearer.o,,drop=FALSE]
    s <- ss[d.nearer.o,,drop=FALSE]
    ddi <- dedupi(s)
    s <- s[ddi,,drop=FALSE]
    d <- d[ddi,,drop=FALSE]
    m[s] <- d.new.d[d.nearer.o][ddi]
  }
  sqrt(m)
}
dist3 <- function(map, water.i, shore.i) {
  m <- array(0, dim(map))
  m[water.i] <- Inf
  s <- shore.i
  d <- matrix(0, nrow(s), 2)
  range <- (-1):1
  # neigh <- as.matrix(subset(expand.grid(x=range, y=range), xor(x,y)))
  neigh <- as.matrix(subset(expand.grid(x=range, y=range), x|y))
  i <- 0
  while(nrow(s)) {
    # writeLines(sprintf("%d rows: %d", i, nrow(s)))
    s.rep <- rep(seq_len(nrow(s)), each=nrow(neigh))
    n.rep <- rep(seq_len(nrow(neigh)), nrow(s))
    nn <- neigh[n.rep,]
    ss <- s[s.rep,] + nn
    inb <-
      ss[,1] > 0 & ss[,1] <= nrow(map) &
      ss[,2] > 0 & ss[,2] <= ncol(map)
    ss <- ss[inb,,drop=FALSE]
    d.new <- d[s.rep,] + nn
    d.new <- d.new[inb,,drop=FALSE]
    d.new.d <- rowSums(d.new ^ 2)
    d.nearer <- which(d.new.d < m[ss])
    d.new.o <- order(d.new.d[d.nearer])
    d.nearer.o <- d.nearer[d.new.o]
    d <- d.new[d.nearer.o,,drop=FALSE]
    s <- ss[d.nearer.o,,drop=FALSE]
    ddi <- dedupi(s)
    s <- s[ddi,,drop=FALSE]
    d <- d[ddi,,drop=FALSE]
    m[s] <- d.new.d[d.nearer.o][ddi]
  }
  sqrt(m)
}
stop()
# md2 <- dist2(map, water.i, shore.i)

map2 <- downsample(map, 50)
water2 <- find_water(map2)
shore2 <- find_shore(water2)
water2.i <- which(water2, arr.ind=TRUE)
shore2.i <- which(shore2, arr.ind=TRUE)

md0 <- dist_brute(map2, water2.i, shore2.i)
md2 <- dist2(map2, water2.i, shore2.i)
m <- md2
m2 <- sqrt(md2); m2[m2 == 0] <- NA; plot_rast(m2, range(m2, na.rm=TRUE));
m2 <- md0; m2[m2 == 0] <- NA; plot_rast(m2, range(m2, na.rm=TRUE));

m2 <- m; m2[m2 == 0] <- NA; plot_rast(m2, c(1, 10), rast=FALSE)
md2[md2 == 0] <- NA

mdiff <- abs(md0 - md2)
range(mdiff)
mdiff[mdiff==0] <- NA
plot(shore2.i)
plot_rast(mdiff, range(mdiff, na.rm=TRUE), rast=FALSE)

points(s, cex=.25, col='red'); text(s, labels=paste0(d[,1],',',d[,2]), cex=.5)

water <- find_water(map)
shore <- find_shore(water)
water.i <- which(water, arr.ind=TRUE)
shore.i <- which(shore, arr.ind=TRUE)

system.time(m2 <- dist2(map, water.i, shore.i))
system.time(m0 <- dist_brute(map, water.i, shore.i))
system.time(m02 <- dist_brute2(map, water.i, shore.i))
system.time(m03 <- dist_brute3(map, water.i, shore.i))
md[md == 0] <- NA
plot_rast(md, range(md, na.rm=TRUE))

dyn.unload('content/post/2021-01-23-out-of-r-depth/script/depth.so')
xx <- dyn.load('content/post/2021-01-23-out-of-r-depth/script/depth.so')
mc <- sqrt(.Call('BG_calc_depth', map2, shore2.i, water2.i))

system.time(mc0 <- sqrt(.Call('BG_calc_depth', map, shore.i, water.i)))
system.time(mc1 <- sqrt(.Call('BG_calc_depth2', map, shore.i, water.i)))
system.time(mc2 <- sqrt(.Call('BG_calc_depth3', map, shore.i, water.i)))

mc0[!water] <- 0

x <- runif(1e7)
a <- 1.001

microbenchmark::microbenchmark(
  .Call('BG_add_scalar', x, a), x + a
)




mdiff1 <- abs(mm - mm2.1)
mdiff2 <- abs(mm - mm2.2)

plot(mdiff1 / abs(mm),  mdiff2 / abs(mm))

f <- function(x, max=max(x)) x / max

library(vetr)

m2 <- m; m2[m2 == 0] <- NA; plot_rast(m2, c(1, 10), rast=FALSE)
plot_rast(mm, invert=TRUE)
plot_rast(mdiff1)
plot_rast(mdiff2)

as.raster((mm - min(mm))/diff(range(mm)))

## Change values to colors
##
## Key thing is that NA values are left NA, so you can combine multiple runs of
## this with different palettes.
##
## @param water logical of same dimensions as `map` designating which elements
##   in `map` should be colored with the water palette.

f <- function(a, b) {
  vetr(
    NUM, LGL.1 && length(.) == length(map),
    list(land=character(), water=character())
  )
}

map_to_col <- function(
  map, water,
  colors=list(
    land=terrain.colors(10),
    water=c('blue4', 'dodgerblue4', 'dodgerblue')
  )
) {
  vetr(
    matrix(numeric(), 0, 0), LGL && length(.) == length(map),
    list(land=character(), water=character())
  )
  m_to_c_int <- function(map, cols) {
    cc <- colorRamp(cols, space='Lab')
    rgb <- cc((map - min(map, na.rm=TRUE))/diff(range(map, na.rm=TRUE)))
    col <- rep(NA_character_, length(map))
    col[!is.na(map)] <- rgb(rgb[!is.na(map),], maxColorValue=255)
    col
  }
  height <- map
  height[water] <- NA
  height.c <- m_to_c_int(height, colors[['land']])

  depth <- map
  depth[!water] <- NA
  depth.c <- m_to_c_int(depth, colors[['water']])

  all.col <- height.c
  all.col[!is.na(depth.c)] <- depth.c[!is.na(depth.c)]
  structure(array(all.col, dim(map)), class='raster')
}
mbrute <- map
mbrute[water] <- mm[water]

m1 <- map
m1[water] <- mm2.1[water]

m2 <- map
m2[water] <- mm2.2[water]

mdd <- map
mdd[water] <- sqrt(md2[water])

colors <- list(
  land=c('chartreuse4', 'wheat3', 'whitesmoke'),
  water=rev(c('blue4', 'dodgerblue4', 'dodgerblue'))
)

plot(map_to_col(mbrute, water, colors=colors))
plot(map_to_col(m1, water, colors=colors))
plot(map_to_col(m2, water, colors=colors))
plot(map_to_col(mdd, water, colors=colors))





depth <- mm
depth[depth <= 0] <- NA
depth <-
  (1 - (depth - min(depth, na.rm=TRUE)) / diff(range(depth, na.rm=TRUE))) ^2
d.col <- map_to_col(
  depth, 
)
height <- map
height[!is.na(depth)] <- NA
h.col <- map_to_col(height, terrain.colors(10))



plot(all.col)

depth <- mm2.1
depth[depth <= 0] <- NA
depth <-
  (1 - (depth - min(depth, na.rm=TRUE)) / diff(range(depth, na.rm=TRUE))) ^2
d.col <- map_to_col(
  depth, c('blue4', 'dodgerblue4', 'dodgerblue')
)
height <- map
height[!is.na(depth)] <- NA
h.col <- map_to_col(height, terrain.colors(10))

all.col <- h.col
all.col[!is.na(d.col)] <- d.col[!is.na(d.col)]

plot(all.col)


mapc <- 


x <- mdiff2

plot(as.raster(1 - mdiff1 / max(mdiff1)))
plot(as.raster(1 - mdiff2 / max(mdiff1)))




(sum(water)+0) * sum(shore)


plot(as.raster(mat == lake))


# idx <- as.matrix(subset(expand.grid(x=-1:1, y=-1:1), xor(x, y)))
cbind(idx, d=sqrt(rowSums(idx ^ 2)))


landi <- which(land, arr.ind=TRUE)
idx2 <- landi[rep(seq_len(nrow(landi)), each=nrow(idx)), ] +
  do.call(rbind, replicate(nrow(landi), idx, simplify=FALSE))

# Use NAs to track oob, we'll make them land later
idx2[idx2 == 0L] <- NA
idx2[idx2[,1] > nrow(land), 1] <- NA
idx2[idx2[,2] > ncol(land), 2] <- NA
land.adj <- matrix(land[idx2], nrow(idx))
land.adj[is.na(land.adj)] <- TRUE

# Compute which lands are adjacent to water
land.adj.water <- which(colSums(land.adj) < nrow(idx))

# Too many, so sample randomly a subset that should be good enough
set.seed(123)
land.adj.w2 <- sample(land.adj.water, length(land.adj.water) / 2)

lc <- t(landi[land.adj.w2,])
wc <- t(which(!land, arr.ind=TRUE))
wlcc <- expand.grid(w=seq_len(ncol(wc)), l=seq_len(ncol(lc)))
wldist <- colSums((wc[, wlcc[['w']]] - lc[, wlcc[['l']]]) ^ 2)
dim(wldist) <- c(ncol(wc), ncol(lc))
nearl <- max.col(-wldist, ties.method='first')
depth <- array(NA_real_, dim=dim(der))
depth.dat <- sqrt(colSums((wc - lc[,nearl]) ^ 2))
depth[t(wc)] <- (depth.dat - min(depth.dat)) / diff(range(depth.dat))
saveRDS(depth, '~/Downloads/derwent/depth2.RDS')


