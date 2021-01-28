source('content/post/2021-01-23-out-of-r-depth/script/utils.R')

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
  #neigh <- as.matrix(subset(expand.grid(x=range, y=range), xor(x,y)))
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

dist4 <- function(map, water.i, shore.i) {
  # initialize output with buffer to detect OOB
  m <- array(0, dim(map) + c(2,2))
  m[c(1,nrow(m)),] <- -Inf
  m[,c(1,ncol(m))] <- -Inf

  w <- water.i + 1.0
  m[w] <- Inf
  s <- shore.i + 1.0
  dxy <- matrix(0, nrow(s), 2)

  rot <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), 2)

  range <- (-1):1
  dir <- as.matrix(subset(expand.grid(x=range, y=range), x | y))
  off <- seq_len(nrow(dir))

  dir.ccw <- round(t(rotr %*% t(dir)))
  dir.cw <- round(t(t(rotr)%*% t(dir)))

  offt <- aperm(
    array(cbind(dir, dir.ccw, dir.cw), c(nrow(dir), 2, 3)), c(3, 1, 2)
  )
  offti <- matrix(
    off[match(offt[,,1] + 3 * offt[,,2], dir[,1] + 3 * dir[,2])], 3
  )
  # Initial move in all directions
  i <- rep(seq_len(nrow(s)), each=8)
  j <- rep(seq_len(8), nrow(s))
  move <- offt[1,,][j,]
  off <- offti[1,j]

  while(nrow(s)) {
    # writeLines(sprintf("%d rows: %d", i, nrow(s)))
    s <- s[i,] + move
    dxy <- dxy[i,] + move
    d <- dxy[,1]^2 + dxy[,1]^2

    # Nearer points, and dedup
    near <- which(d < m[s])
    dn <- d[near]
    dno <- order(dn)
    dnoi <- near[dno]
    s <- s[dnoi,,drop=FALSE]
    k <- dedupi(s)
    s <- s[k,,drop=FALSE]
    dnoik <- dnoi[k]
    dxy <- dxy[dnoik,,drop=FALSE]
    off <- off[dnoik]

    # record new values
    m[s] <- d[dnoik]

    # setup next move
    move <- offt[,off,]
    dim(move) <- c(length(off) * 3, 2)
    off <- c(offti[,off])
    i <- rep(seq_len(nrow(s)), each=3)
  }
  sqrt(m[-c(1,nrow(m)), -c(1,ncol(m))])
}

