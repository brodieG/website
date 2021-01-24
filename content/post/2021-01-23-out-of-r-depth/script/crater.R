source('static/script/_lib/rayrender.R')
source('static/script/_lib/plot.R')

library(rayrender)
library(ambient)    # for water patterns
# http://oe.oregonexplorer.info/craterlake/dem.html
eltif <- raster::raster("~/Downloads/dems_10m.dem")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
# not memory efficient but oh well, can't be bothered learning
# how to do this properly
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

map <- elmat1[1500:2599 + 150, 1500:2599]

neighbors <- as.matrix(subset(expand.grid(x=-1:1, y=-1:1), x | y))
neighbors2 <- as.matrix(subset(expand.grid(x=-4:4, y=-4:4), x | y))
# neighbors2 <- neighbors
neighbors2 <- cbind(neighbors2, sqrt(rowSums(neighbors2^2)))

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
samp <- 50
ri <- seq(1, nrow(map), by=samp)
ci <- seq(1, ncol(map), by=samp)
map2 <- matrix(map[as.matrix(expand.grid(ri, ci))], length(ri))
plot(as.raster((map2 - min(map2)) / diff(range(map2))))

water <- find_water(map)
shore <- find_shore(water)

water.i <- which(water, arr.ind=TRUE)
shore.i <- which(shore, arr.ind=TRUE)

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
mm <- dist_brute(map2, water.i, shore.i)
plot(as.raster((mm - min(mm)) / diff(range(mm))))

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
    writeLines(sprintf("%d rows: %d", i, nrow(s)))
    ss <- t(
      t(s[rep(seq_len(nrow(s)), each=nrow(neigh)),]) +
      c(t(neigh[,1:2]))
    )
    inb <-
      ss[,1] > 0 & ss[,1] <= nrow(map) &
      ss[,2] > 0 & ss[,2] <= ncol(map)
    ss <- ss[inb,]
    d.new <- rep(d, each=nrow(neigh)) + neigh[,3]
    d.new <- d.new[inb]
    d.nearer <- which(d.new < m[ss])
    d.new.near <- d.new[d.nearer]
    d.new.o <- order(d.new.near)
    d <- d.new.near[d.new.o]
    s <- ss[d.nearer[d.new.o],]
    s.dup <- duplicated(s)
    s <- s[!s.dup,,drop=FALSE]
    d <- d[!s.dup]
    m[s] <- d
  }
  m
}
vcf <- colorRamp(viridis(256), space='Lab')
vc <- function(x) rgb(vcf((x - min(x)) / diff(range(x))), maxColorValue=255)

mm2 <- dist1(map, water.i, shore.i)
mm2[is.na(mm2) | !is.finite(mm2)] <- 0
plot(as.raster((mm2 - min(mm2)) / diff(range(mm2))))





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


