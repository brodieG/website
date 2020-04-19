# Logo stuff, very specific to this file and the SVG paths it contains, don't
# assume this will work with anything else.

logo.page <- readLines('https://www.r-project.org/logo/Rlogo.svg')
dat <-regmatches(logo.page, gregexpr("\"[^\"]+\"", logo.page))
path <- unlist(strsplit(sub('^"(.*)"$', '\\1', dat[[13]][[1]]), " "))
path <- path[-length(path)]  # drop "Z" command at end
cmd <- sub("^([A-Z]*).*$", "\\1", path)
x <- sub("[^0-9]*([0-9.]*),.*$", "\\1", path)
y <- sub(".*,([0-9.]*).*$", "\\1", path)

d <- data.frame(i=seq_along(x), x=as.numeric(x), y=as.numeric(y), cmd=cmd)
d[2:3] <- lapply(d[2:3], function(x) (x - min(x)) / diff(range(x)))

library(gridBezier)

brle <- rle(cmd == "C" | cmd == "")
bends <- with(brle, cumsum(lengths)[values])
bstarts <- with(brle, cumsum(lengths)[!values])

bzs <- Map(
  function(start, end) {
    points <- d[start:end, c('x', 'y')]
    BezierGrob(points[[1]], points[[2]], stepFn=nSteps(20))
  },
  bstarts,
  bends
)
bzsp <- lapply(bzs, BezierPoints)
bzsp <- lapply(
  bzsp, function(z) list(z[[1]] / par()$pin[[1]], z[[2]] / par()$pin[[2]])
)

# reconnect with the line segments

lrle <- rle(cmd == "L")
lends <- with(lrle, cumsum(lengths)[values])
lstarts <- with(lrle, cumsum(lengths)[!values][seq_along(lends)])

lnsp <- Map(
  function(start, end) d[start:end, c('x', 'y')],
  lstarts,
  lends
)
lns <- lapply(
  lnsp,
  function(x) grid.lines(x[[1]], x[[2]], gp=gpar(col='#000000'))
)
out <- c(bstarts < 33, lstarts < 33)
outside <- c(bzsp, lnsp)[out][order(c(bstarts, lstarts)[out])]
inside <- c(bzsp, lnsp)[!out][order(c(bstarts, lstarts)[!out])]

# plot.new()
# for(points in list(outside, inside)) {
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
