source('static/script/mesh-viz/rtin-vec2.R')
source('static/script/mesh-viz/rtin-vec.R')
source('static/script/mesh-viz/extract-vec.R')
source('static/script/mesh-viz/viz-lib.R')
source('static/script/_lib/plot.R')

library(grid)
library(ggplot2)
library(watcher)
library(reshape2)
library(gtable)
vars <- c(
  'o', 'o.m', 'o.a', 'o.b',  'o.dim', 'errors', 'err.i',
  'j', 'i', 'err.n', 'tile.n'
)
errors_watched <- watch(compute_error3b, vars)
m <- map[1:17, 1:17]
# m <- map[1:5, 1:5]
# m <- map
err.test <- compute_error3b(m)
err.test[is.na(err.test)] <- 0
all.equal(compute_error2a(m), err.test)
xx <- errors_watched(m)
xx2 <- lapply(
  attr(xx, 'watch.data'),
  function(x) {
    if('o' %in% names(x)) x[['o']] <- c(x[['o']])
    x
  }
)
zz.raw <- simplify_data(xx2)
size.max <- max(xx, na.rm=TRUE)

id.vars <- c('.id', '.line')
zz.vec <- zz.raw[['.scalar']]

# - Code -----------------------------------------------------------------------

code <- deparse(compute_error3b, control='all')
code[c(1L, length(code))] <- ""
code <- sub("^ ", "", code)
code.rez <- vector('list', length(zz.vec$.id))

cwindow <- 54
coff <- cwindow / 2
cbuff <- 2
cend <- max(zz.vec$.line)

for(i in seq_along(zz.vec$.id)) {
  id <- zz.vec$.id[i]
  line <- zz.vec$.line[i]

  if(line - coff > cwindow - cbuff || line < coff) {
    coff <- min(line - cbuff, cend - cwindow)
  }
  code.rez[[i]] <- data.frame(
    y=-(seq_along(code) - coff) + cwindow / 2,
    .id=rep(i, length(code)),
    code=code,
    highlight=ifelse(seq_along(code) == line, 'white', 'black')
  )
}
code.fin <- transform(do.call(rbind, code.rez), frame='Coords')

# - Viz ------------------------------------------------------------------------

# We need
# * to color based on o.m, o.b, o.a?  Or maybe better start colored.  And use
#   err.list for the circles and arrows.  Circles for all, arrows from 2nd+ to
#   1st.
#
# One issue with this approach is that we don't get to see the hypotenuse
# highlighted.  Maybe instead use o.a/o.b to draw the hypotenuse.  Colors are
# there from the get-go, but maybe the outline of the hypotenuse is what shows
# up.
#
# Circles show up when ids go into err.list.
#
# Urgh, we're probably not going to do this with the faster version.


errors <- transform(
  zz.raw$errors, x=(x - 1) / (ncol(m) - 1), y=(y - 1) / (nrow(m) - 1),
  frame='Errors', stringsAsFactors=FALSE
)

# Colored points

o <- transform(
  ids_to_df(zz.raw$o$val, m),
  frame='Coords', .id=zz.raw$o$.id,
  id=zz.raw$o$val, stringsAsFactors=FALSE
)
id.scalar <- match(o$.id, zz.vec$.id)
o <- transform(o, j=zz.vec[id.scalar, 'j'], n=ave(.id, .id, FUN=length))
o <- transform(o, reps=n / ((zz.vec$err.n[id.scalar]) + 2L))
o <- transform(o, i=ave(.id, .id, FUN=seq_along))
ptypes <- c(rep('end', 2), 'mid', rep('child', 5))
pcolors <- c(rep('#66c2a5', 2), '#8da0cb', rep('#fc8d62', 5))
pfills <- pcolors
pfills[ptypes == 'end'] <- 'grey90'
psizes <- c(rep(1/6, 2L), 1/3, rep(1/10, 5)) * size.max
o <- transform(
  o,
  ptype=ptypes[(i - 1) %/% reps + 1],
  pcolor=pcolors[(i - 1) %/% reps + 1],
  psize=psizes[(i - 1) %/% reps + 1],
  pfill=pfills[(i - 1) %/% reps + 1]
)

# Boundary around colored points + hypotenuse

o.p <- with(zz.raw,
  rbind(
    cbind(o.m, ptype='mid'),
    cbind(o.a, ptype='end'),
    cbind(o.b, ptype='end')
) )
o.p <- cbind(o.p, ids_to_df(o.p$val, m))
o.p[['frame']] <- 'Coords'

o.a <- with(zz.raw$o.a, cbind(.id, ids_to_df(val, m)))
o.b <- with(
  zz.raw$o.b, cbind(.id, setNames(ids_to_df(val, m), c('xend','yend','zend')))
)
o.a <- transform(o.a, i=ave(.id, .id, FUN=seq_along))
o.b <- transform(o.b, i=ave(.id, .id, FUN=seq_along))
o.ab <- merge(o.a, o.b, by=c('.id', 'i'), all=TRUE)
o.ab[['frame']] <- 'Coords'
o.m <- cbind(zz.raw$o.m, ids_to_df(zz.raw$o.m$val, m))
# arrows

err.arrows <- do.call(rbind,
  lapply(
    seq_along(zz.raw$err.i),
    function(i) {
      x <- zz.raw$err.i[[i]]
      if(length(x) > 1 && length(x[[1]]) && sum(lengths(x[-1]) > 0)) {
        a <- rep(x[[1]], sum(lengths(x[-1]) > 0))
        data.frame(.id=zz.vec$.id[i], start=unlist(x[-1]), end=a)
} } ) )
err.arrows <- with(err.arrows,
  cbind(
    .id, ids_to_df(start, m),
    setNames(ids_to_df(end, m), c('xend','yend','zend'))
) )
err.arrows <-
  transform(err.arrows, diag=ifelse(xend - x & yend - y, sqrt(2), 1))
arrow.mult <- log2(nrow(m) - 1) * 13
err.arrows <- transform(
  err.arrows,
  xend=xend - sign(xend - x) * 1 / (arrow.mult * diag),
  yend=yend - sign(yend - y) * 1 / (arrow.mult * diag)
)
# compile plot data for use

data <- list(
  o=o, errors=errors, o.m=o.m,
  o.p=o.p, o.ab=o.ab, err.arrows=err.arrows,
  code=code.fin
)
data <- lapply(
  data,
  function(x) {
    if('frame' %in% names(x))
      x[['frame']] <- factor(x[['frame']], levels=c('Coords', 'Errors'))
    x
} )
# sizes for 9 x 9 / 5 x 5

range.max <- 10
circle.max <- 18
arrow.size <- unit(0.25, 'inches')

# sizes for 17 x 17

range.max <- 6
circle.max <- 8
arrow.size <- unit(0.08, 'inches')

# text adjustments

y.shift <- 0.2
y.mult <- 2.5
x.shift <- 1.40

dpi <- 72
width <- 600
# height <- width
height <- width / 7 * 4.037974
# dev.new(width=width/dpi, height=height/dpi)


thm.blnk <- list(
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
    # plot.margin=unit(c(.1, 0, .1, (height/2)/dpi + 0.1), "inches"),
    panel.grid=element_blank()
  ),
  ylab(NULL),
  xlab(NULL)
)
bg.point <- ids_to_df(seq_along(m), m)
frames <- seq_len(nrow(zz.vec))
library(ggplot2)
k <- 82
for(k in frames) {
cat(sprintf("\rFrame %04d", k))
d <- lapply(data, function(x) subset(x, .id == k))

p <- ggplot(mapping=aes(x, y)) +
  geom_point(data=bg.point, color='white', size=.5, shape=3) +
  geom_point(
    data=data.frame(
      x=0, y=0,
      frame=factor(c('Coords', 'Errors'), levels=c('Coords', 'Errors'))
    ), alpha=0
  ) +
  geom_segment(data=d$o.ab, aes(xend=xend, yend=yend)) +
  geom_point(data=d$o.m, shape=21L, fill=NA, size=circle.max) +
  geom_segment(
    data=d$err.arrows, aes(xend=xend, yend=yend),
    arrow=arrow(type='closed', length=arrow.size), color='grey65'
  ) +
  geom_point(
    data=d$o, aes(color=I(pcolor), fill=I(pfill), size=psize), shape=21
  ) +
  geom_point(data=d$errors, aes(size=val)) +
  scale_size(
    limits=c(0, size.max), range=c(0,range.max), guide=FALSE
  ) +
  # geom_label(
  #   data=subset(d$code, highlight=='white'),
  #   aes(
  #     x=-x.shift,
  #     y=y/cwindow*y.mult + y.shift, label=code
  #   ),
  #   hjust=0, family='mono', color=NA, fill='grey20',
  #   label.padding = unit(0.15, "lines"),
  # ) +
  # geom_text(
  #   data=d$code,
  #   aes(
  #     x=-x.shift,
  #     y=y/cwindow*y.mult + y.shift, label=code
  #  ),
  #   hjust=0, family='mono', color=d$code$highlight
  # ) +
  coord_fixed(ylim=c(0,1), xlim=c(0,1), clip="off") +
  thm.blnk +
  facet_wrap(~frame) +
  # facet_wrap(~frame, ncol=1) +
  ggtitle(
    sprintf(
      paste0(
        'Frame %0', as.integer(log10(max(frames))) + 1,
        'd/%d (i: %d, j: %s, tile.n: %d)'
      ),
      k, max(frames), zz.vec[k, 'i'], zz.vec[k, 'j'], zz.vec[k, 'tile.n']
    )
  ) +
  NULL
p
# stop('pause')
# pdim <- gtable_dim(ggplotGrob(p))
ggsave(
  filename=sprintf('~/Downloads/mesh-anim-5a/img-%04d.png', k),
  plot=p,
  width=width/dpi, height=height/dpi, units='in', device='png',
  dpi=dpi
)
}

stop('done plot')

