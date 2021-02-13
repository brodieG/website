# source('content/post/2021-01-23-out-of-r-depth/script/crater.R')
library(vetr)
library(viridisLite)
library(rayrender)
library(ambient)    # for water patterns
source('static/script/_lib/rayrender.R')
source('static/script/_lib/plot.R')
source('content/post/2021-01-23-out-of-r-depth/script/dist.R')
source('content/post/2021-01-23-out-of-r-depth/script/plot.R')

# http://oe.oregonexplorer.info/craterlake/dem.html
# eltif <- raster::raster("~/Downloads/dems_10m.dem")
# eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
# not memory efficient but oh well, can't be bothered learning
# how to do this properly
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

map <- elmat1[1500:2599 + 150, 1500:2599]
water <- find_water(map)
table(water)

stop()
# md2 <- dist2(map, water.i, shore.i)

mini <- matrix(c(
  1,1,1,1,1,1,1,1,1,
  1,1,0,0,0,0,0,0,1,
  1,0,0,0,0,0,0,0,1,
  1,0,0,0,0,0,0,0,1,
  1,0,0,0,0,0,0,0,1,
  1,0,0,0,0,0,0,0,1,
  1,1,1,1,1,1,1,1,1), nrow=9)

water3 <- mini == 0
shore3 <- find_shore(water3)

w3i <- which(water3, arr.ind=TRUE)
s3i <- which(shore3, arr.ind=TRUE)
dist4(mini, w3i, s3i)

dist4w <- watcher::watch(dist4, c('s.fin', 'move'))
res <-  dist4w(mini, w3i, s3i)

watch <- attr(m2, 'watch.data')
coords <- lapply(watch, '[[', 's.fin')
s.delt <- mapply(identical, coords[-length(coords)], coords[-1])
paths <- lapply(
  which(!s.delt) + 2,
  function(i)
    with(
      watch[[i]],
      rbind(
        s.fin - move, s.fin,
        array(rep(NA, length(s.fin)), dim(s.fin))
    )[
      order(rep(seq_len(nrow(s.fin)), 3)),
  ] )
)
p.all <- do.call(rbind, paths)
p.rng <- apply(p.all, 2, range, na.rm=TRUE) + c(-1, 1)

for(i in seq_along(paths)) {
  png(
    width=800, height=800, filename=next_file("~/Downloads/depth/img-000.png")
  )
  par(mai=numeric(4))
  plot.new()
  plot.window(xlim=p.rng[,1], ylim=p.rng[,2])
  lines(do.call(rbind, paths[seq_len(i)]))
  dev.off()
}



lines(p.all)


map2 <- downsample(map, 2)
water2 <- find_water(map2)
shore2 <- find_shore(water2)
water2.i <- which(water2, arr.ind=TRUE)
shore2.i <- which(shore2, arr.ind=TRUE)
m2 <- dist4w(map2, water2.i, shore2.i[1275,,drop=FALSE])
dist4(map2, water2.i, shore2.i[1,,drop=FALSE])


m4 <- dist4(map, water.i, shore.i)

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

system.time(m4 <- dist4(map, water.i, shore.i))
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


# Let's make some skinny lakes

mskinny.raw <- downsample(map, c(2, 5))
mskinny <- rbind(
  do.call(cbind, replicate(5, mskinny.raw, simplify=FALSE)),
  do.call(cbind, replicate(5, mskinny.raw, simplify=FALSE))
)
msw <- find_water(mskinny)
mss <- find_shore(msw)
mswi <- which(msw, arr.ind=TRUE)
mssi <- which(mss, arr.ind=TRUE)

ms <- dist4(mskinny, mswi, mssi)
system.time(mcs <- sqrt(.Call('BG_calc_depth', mskinny, mssi, mswi)))
mcs[mcs == Inf] <- 0



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

mbrute <- map
mbrute[water] <- mm[water]

m1 <- map
m1[water] <- mm2.1[water]

m2 <- map
m2[water] <- mm2.2[water]

mdd <- map
mdd[water] <- sqrt(md2[water])


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


