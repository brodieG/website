# source('content/post/2021-01-23-out-of-r-depth/script/utils.R')
source('script/utils.R')

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
  ## Initialize output with buffer to detect OOB.
  m <- array(-Inf, dim(map) + c(2,2))
  m[-c(1,nrow(m)),-c(1,ncol(m))] <- 0

  ## Adjust coords for buffer
  w <- water.i + 1.0  # calculation faster with num
  s <- shore.i + 1.0  # instead of int
  m[w] <- Inf
  dxy <- matrix(0, nrow(s), 2)  # Coords from nearest shore

  ## Setup offset moves.
  rot <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), 2)
  dir <- as.matrix(expand.grid(x=-1:1, y=-1:1))
  dir.ccw <- round(t(rot %*% t(dir)))
  dir.cw <- round(t(t(rot)%*% t(dir)))
  dirs <- cbind(dir, dir.ccw, dir.cw)

  ## For each position, there are 8 directions, but we narrow
  ## that to 3: original direction (from), and +- 45 degrees.
  ## Each column in `moves` represents one of the possible
  ## 9 directions, and the values are the three possible
  ## subsequent directions.
  moves <- aperm(array(dirs, c(nrow(dir), 2, 3)), c(3, 1, 2))
  froms <- moves[,,1] +  moves[,,2] * 3 + 5

  ## Except for first step: initial move in all directions
  i <- rep(seq_len(nrow(s)), each=nrow(dir))
  j <- rep(seq_len(nrow(dir)), nrow(s))
  move <- moves[1,,][j,]
  from <- froms[1,j]
  count <- 0

  while(nrow(s)) {
    s <- s[i,] + move           # Current position
    dxy <- dxy[i,] + move       # Vector from shore
    d <- dxy[,1]^2 + dxy[,2]^2

    ## Identify nearest and order by distance
    near <- which(d < m[s])
    dn <- d[near]
    dno <- order(dn)
    dnoi <- near[dno]
    s <- s[dnoi,,drop=FALSE]

    ## Dedup, keeping nearest at each coordinate
    ui <- which(dedupi(s))
    dnoui <- dnoi[ui]
    s.fin <- s <- s[ui,,drop=FALSE]
    move <- move[dnoui,]
    dxy <- dxy[dnoui,,drop=FALSE]
    from <- from[dnoui]
    m[s] <- d[dnoui]

    ## Setup next move
    move <- moves[,from,]
    dim(move) <- c(length(from) * 3, 2)
    from <- c(froms[,from])
    i <- rep(seq_len(nrow(s)), each=3)
  }
  sqrt(m[-c(1,nrow(m)), -c(1,ncol(m))])
}

