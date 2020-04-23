# - Basic Processing -----------------------------------------------------------

# Logo stuff, very specific to this file and the SVG paths it contains, don't
# assume this will work with anything else.

logo.page <- readLines('https://www.r-project.org/logo/Rlogo.svg')
dat <-regmatches(logo.page, gregexpr("\"[^\"]+\"", logo.page))

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

dev.new(width=4, height=4)
plot.new()
par(mai=numeric(4))
grid.draw(grid.lines(c(0, 0, 1, 1), c(0, 1, 1, 0)))

bzsp <- lapply(bzs, BezierPoints)
bzspi <- lapply(bzsp, lapply, unit, 'inches')
bzsp <- lapply(
  bzspi, function(x) list(convertX(x[[1]], 'npc'),convertY(x[[2]], 'npc'))
)
bzsp <- lapply(
  bzsp, function(z) list(z[[1]] / par()$pin[[1]], z[[2]] / par()$pin[[2]])
)
dev.off()
#
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

ind <- decido::earcut(cbind(allx, ally), holes=length(outx) + 1)
decido::plot_ears(cbind(allx, ally), ind, col = "#1e64b6")
