# source('static/post/2020-02-13-on-nse_files/scripts/svg-import.R')
# - Basic Processing -----------------------------------------------------------

# Logo stuff, very specific to this file and the SVG paths it contains, don't
# assume this will work with anything else.

logo.page <- readLines('https://www.r-project.org/logo/Rlogo.svg')
dat <-regmatches(logo.page, gregexpr("\"[^\"]+\"", logo.page))

dat <-

## Parse SVG Path Data
##
## Letter, space, comma and hyphen delimited x y coordinate pairs.
##
## @param x an svg path

x <- "M210,288.5c0-10.1-0.2-18.8-0.7-26.8h12.9l0.5,16.9h0.7c3.7-11.6,12.6-18.8,22.5-18.8 c1.4,0,2.8,0.2,4.2,0.5V275c-1.6-0.4-3.3-0.6-5-0.5c-10.4,0-17.8,8.4-19.8,20.1c-0.4,2.4-0.6,4.8-0.7,7.3v45.9H210V288.5z"

path_components <- function(x) {
   pieces <- unname(split(x, cumsum(grepl("[a-zA-Z]", x))))
   lapply(pieces, function(y) list(y[1], as.numeric(y[-1])))
}
# Convert Path To Absolute Curve and Lines
#
# @param list of lists, each sub-lists contains the command type (e.g. M, C, L,
#   V, H, Z, m, c, l, v, h, z) at position one, and then a set of coordinates as
#   a numeric vector.
# @param x starting x coordinate
# @param y starting y coordinate
# @return a list of lists similar to the input, except the only commands therein
#   will be M, C, L, and Z, and coordinates will be a nx2 matrix of x-y
#   coordinates.

process_components <- function(path, x=0, y=0) {
  invalid_cmd <- function(i, cmd) stop("Invalid ", cmd, " command at index ", i)
  x0 <- x
  y0 <- y
  if(identical(toupper(path[[1]][[1]]), "M")) {
    if(length(path[[1]][[2]]) != 2) invalid_cmd(1, path[[1]][[1]])
    x0 <- path[[1]][[2]][1]
    y0 <- path[[1]][[2]][2]
  }
  for(i in seq_along(path)) {
    el <- path[[i]]
    if(!is.character(el[[1]]) || length(el[[1]]) != 1L) stop("Invalid command")
    path[[i]] <- switch(
      el[[1]],
      m=, l={
        if(length(el[[2]]) != 2) invalid_cmd(i, el[[1]])
        x <- x + el[[2]][1]
        y <- y + el[[2]][2]
        list(toupper(el[[1]]), matrix(c(x, y), nrow=1))
      },
      c={
        if(length(el[[2]]) %% 2) invalid_cmd(i, el[[1]])
        res <- list(
          "C",
          cbind(
            cumsum(el[[2]][c(TRUE,FALSE)]) + x,
            cumsum(el[[2]][c(FALSE,TRUE)]) + y
        ) )
        x <- res[[2]][nrow(res[[2]]), 1]
        y <- res[[2]][nrow(res[[2]]), 2]
        res
      },
      v=,h={
        if(length(el[[2]]) != 1) invalid_cmd(i, el[[1]])
        if(el[[1]] == 'v') y <- y + el[[2]]
        else x <- x + el[[2]]
        list("L", matrix(c(x, y), nrow=1))
      },
      z=,Z={
        if(length(el[[2]])) invalid_cmd(i, el[[1]])
        x <- x0
        y <- y0
        list("L", matrix(c(x, y), nrow=1))
      },
      M=,L={
        if(length(el[[2]]) != 2) invalid_cmd(i, el[[1]])
        x <- el[[2]][1]
        y <- el[[2]][2]
        list(el[[1]], matrix(c(x, y), nrow=1))
      },
      C={
        if(length(el[[2]]) %% 2) invalid_cmd(i, el[[1]])
        res <- list("C", matrix(el[[2]], ncol=2, byrow=TRUE))
        x <- res[[2]][nrow(res[[2]]), 1]
        y <- res[[2]][nrow(res[[2]]), 2]
        res
      },
      V=,H={
        if(length(el[[2]]) != 1) invalid_cmd(i, el[[1]])
        if(el[[1]] == 'V') y <- el[[2]]
        else x <- el[[2]]
        list("L", matrix(c(x, y), nrow=1))
      },
      stop("unknown command ", i[[1]])
    )
  }
  path
}
# Assumes all paths are closed

parse_path <- function(x) {
  if(!is.character(x) || length(x) != 1) stop("Input not character(1L)")
  raw <- regmatches(x, gregexpr("-?[0-9.]+|[a-zA-Z]", x))[[1]]
  raw <- unname(split(raw, cumsum(grepl("[a-zA-Z]", raw))))
  cmds <- lapply(
    raw, function(x) {
      if(length(x)) list(x[1], as.numeric(x[-1]))
      else list()
  } )
  process_components(cmds)
}

get_path <- function(dat) {
  path <- unlist(strsplit(sub('^"(.*)"$', '\\1', dat), " "))
  path <- path[-length(path)]  # drop "Z" command at end
  cmd <- sub("^([A-Z]*).*$", "\\1", path)
  x <- sub("[^0-9]*([0-9.]*),.*$", "\\1", path)
  y <- sub(".*,([0-9.]*).*$", "\\1", path)
  d <- data.frame(i=seq_along(x), x=as.numeric(x), y=as.numeric(y), cmd=cmd)
  d
}
library(gridBezier)

width <- as.numeric(gsub('"', '', dat[[1]][4]))
height <- as.numeric(gsub('"', '', dat[[1]][5]))

dr <- get_path(dat[[13]][[1]])  # R logo
dh <- get_path(dat[[12]][[1]])  # Hula

dr[[2]] <- dr[[2]] / width
dr[[3]] <- dr[[3]] / height
dh[[2]] <- dh[[2]] / width
dh[[3]] <- dh[[3]] / height

# Absolute coordinates in caps
#
# M: moveto
# L: lineto
# Z: closepath
# ZM: cloepath/moveto?
# C: curveto - cubic bezier, which start at the previous point, list the two
#    control point, and then list the end point, so four coordinates, of which
#    one is the last coordinate of the prior command.
#
# Pull out the bezier segments

# - R Path ---------------------------------------------------------------------

brle <- with(dr, rle(cmd == "C" | cmd == ""))
bends <- with(brle, cumsum(lengths)[values])
bstarts <- with(brle, cumsum(lengths)[!values])

bzs <- Map(
  function(start, end) {
    points <- dr[start:end, c('x', 'y')]
    BezierGrob(points[[1]], points[[2]], stepFn=nSteps(20))
  },
  bstarts,
  bends
)
# Can't figure out how to get BezierPoints to not return in inches, so converting
# back to NPC manually, which is a real hack.  Need to make sure `par$pin()` is
# properly set to use, so we init device.

bzsp <- lapply(bzs, BezierPoints)
bzspi <- lapply(bzsp, lapply, unit, 'inches')
bzsp <- lapply(
  bzspi, function(x) list(convertX(x[[1]], 'npc'),convertY(x[[2]], 'npc'))
)
# plot.new(); lapply(bzs, grid.draw)
# reconnect with the line segments

lrle <- with(dr, rle(cmd == "L"))
lends <- with(lrle, cumsum(lengths)[values])
lstarts <- with(lrle, cumsum(lengths)[!values][seq_along(lends)])

lnsp <- Map(
  function(start, end) dr[start:end, c('x', 'y')],
  lstarts,
  lends
)
lns <- lapply(
  lnsp,
  function(x) grid.lines(x[[1]], x[[2]], gp=gpar(col='#000000'))
)
# plot.new(); lapply(lns, grid.draw)

# plot.new()
# for(points in bzsp) {
#   x <- points[[1]]
#   y <- points[[2]]
#   grid.draw(grid.lines(x, y, gp=gpar(col='#000000')))
# }

# reconnect with the line segments
out <- c(bstarts < 33, lstarts < 33)
outside <- c(bzsp, lnsp)[out][order(c(bstarts, lstarts)[out])]
inside <- c(bzsp, lnsp)[!out][order(c(bstarts, lstarts)[!out])]

# plot.new()
# for(points in list(outside)) {
#   x <- unlist(lapply(points, '[[', 1))
#   y <- unlist(lapply(points, '[[', 2))
#   grid.draw(grid.lines(x, y, gp=gpar(col='#000000')))
# }

outx <- unlist(lapply(outside, '[[', 1))
outy <- unlist(lapply(outside, '[[', 2))

inx <- unlist(lapply(inside, '[[', 1))
iny <- unlist(lapply(inside, '[[', 2))

allx <- c(outx, inx)
ally <- -c(outy, iny) + 1

# - Hula -----------------------------------------------------------------------

# This one is all beziers

hbrle <- with(dh, rle(cmd == "C" | cmd == ""))
hbends <- with(hbrle, cumsum(lengths)[values])
hbstarts <- with(hbrle, cumsum(lengths)[!values])

hbzs <- Map(
  function(start, end) {
    points <- dh[start:end, c('x', 'y')]
    BezierGrob(points[[1]], points[[2]], stepFn=nSteps(20))
  },
  hbstarts,
  hbends
)
hbzsp <- lapply(hbzs, BezierPoints)
hbzspi <- lapply(hbzsp, lapply, unit, 'inches')
hbzsp <- lapply(
  hbzspi, function(x) list(convertX(x[[1]], 'npc'),convertY(x[[2]], 'npc'))
)
# plot.new()
# grid.draw(grid.lines(hbzsp[[1]][[1]], hbzsp[[1]][[2]]))
# grid.draw(grid.lines(hbzsp[[2]][[1]], hbzsp[[2]][[2]]))

hulax <- c(hbzsp[[1]][[1]], hbzsp[[2]][[1]])
hulay <- -c(hbzsp[[1]][[2]], hbzsp[[2]][[2]]) + 1

# hind <- decido::earcut(cbind(hulax, hulay), holes=length(hbzsp[[1]][[1]]) + 1)
# decido::plot_ears(cbind(hulax, hulay), hind, col = "#1e64b6")

# - Plotting -------------------------------------------------------------------

# ind <- decido::earcut(cbind(allx, ally), holes=length(outx) + 1)
# decido::plot_ears(cbind(allx, ally), ind, col = "#1e64b6")

# ind <- decido::earcut(cbind(outx, 1-outy))
# decido::plot_ears(cbind(outx, 1-outy), ind, col = "#1e64b6")


