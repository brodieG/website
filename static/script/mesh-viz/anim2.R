source('static/script/mesh-viz/rtin-vec2.R')
source('static/script/mesh-viz/rtin-vec.R')
source('static/script/mesh-viz/extract-vec.R')
source('static/script/mesh-viz/viz-lib.R')

library(watcher)
library(reshape2)
vars <- c(
  'o', 'o.m', 'o.a', 'o.b',  'o.dim', 'errors', 'err.ids', 'c.rep','r.rep',
  'j'
)
errors_watched <- watch(compute_error3b, vars)
m <- map[1:5, 1:5]
xx <- errors_watched(m)
zz.raw <- simplify_data(attr(xx, 'watch.data'))
size.max <- max(xx, na.rm=TRUE)

id.vars <- c('.id', '.line')
zz.vec <- zz.raw[['.scalar']]

# - Code -----------------------------------------------------------------------

code <- deparse(compute_error3b, control='all')
code[[1]] <- ""
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

thm.blnk <- list(
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank()
  ),
  ylab(NULL),
  xlab(NULL)
)

dpi <- 72
width <- 600
height <- width / 2
size <- nrow(map)

errors <- transform(
  zz.raw$errors, x=(x - 1) / (ncol(m) - 1), y=(y - 1) / (nrow(m) - 1),
  frame='Errors', stringsAsFactors=FALSE
)

# Colored points

o <- transform(
  ids_to_df(zz.raw$o$val, m),
  frame='Ids', .id=zz.raw$o$.id,
  id=zz.raw$o$val, stringsAsFactors=FALSE
)
id.scalar <- match(o$.id, zz.vec$.id)
o <- transform(
  o, els=with(zz.vec[id.scalar,], c.rep * r.rep * ifelse(j == 'diag', 4L, 1L)),
  j=zz.vec[id.scalar, 'j']
)
o <- transform(o, i=ave(.id, .id, FUN=seq_along))
ptypes <- c('mid', rep('end', 2), rep('child', 5))
pcolors <- c('#8da0cb', '#66c2a5', '#66c2a5', rep('#fc8d62', 5))
psizes <- c(rep(1/3, 3L), rep(1/10, 5)) * size.max
o <- transform(
  o,
  ptype=ptypes[(i - 1) %/% els + 1],
  pcolor=pcolors[(i - 1) %/% els + 1], 
  psize=psizes[(i - 1) %/% els + 1]
)

# Boundary around colored points + hypotenuse

o.p <- with(zz.raw,
  rbind(
    cbind(o.m, ptype='mid'),
    cbind(o.a, ptype='end'),
    cbind(o.b, ptype='end')
) )
o.p <- cbind(o.p, ids_to_df(o.p$val, m))
o.p[['frame']] <- 'Ids'

o.a <- with(zz.raw$o.a, cbind(.id, ids_to_df(val, m)))
o.b <- with(
  zz.raw$o.b, cbind(.id, setNames(ids_to_df(val, m), c('xend','yend','zend')))
)
o.a <- transform(o.a, i=ave(.id, .id, FUN=seq_along))
o.b <- transform(o.b, i=ave(.id, .id, FUN=seq_along))
o.ab <- merge(o.a, o.b, by=c('.id', 'i'), all=TRUE)
o.ab[['frame']] <- 'Ids'

# Error circle / mappings

# err.ids <- lapply(zz.raw$err.ids, unlist)
# ids.len <- lengths(err.ids)
# err.cir <- cbind(
#   data.frame(.id=rep(zz.vec$.id, ids.len)),
#   ids_to_df(unlist(err.ids), m)
# )
o.m <- cbind(zz.raw$o.m, ids_to_df(zz.raw$o.m$val, m))
# arrows

err.arrows <- do.call(rbind,
  lapply(
    seq_along(zz.raw$err.ids),
    function(i) {
      x <- zz.raw$err.ids[[i]]
      if(length(x) > 1 && length(x[[1]]) && sum(lengths(x[-1]) > 0)) {
        a <- rep(x[[1]], sum(lengths(x[-1]) > 0))
        data.frame(.id=zz.vec$.id[i], start=unlist(x[-1]), end=a)
} } ) )
err.arrows <- with(err.arrows,
  cbind(
    .id, ids_to_df(start, m),
    setNames(ids_to_df(end, m), c('xend','yend','zend')),
    frame='Errors'
) )
# compile plot data for use

data <- list(
  o=o, errors=errors, o.m=o.m,
  o.p=o.p, o.ab=o.ab, err.cir=err.cir, err.arrows=err.arrows
)


frames <- 15:50
library(ggplot2)
# for(k in frames) {
k <- 60
cat(sprintf("\rFrame %04d", k))
d <- lapply(data, function(x) subset(x, .id == k))

p <- ggplot(mapping=aes(x, y)) +
  geom_segment(data=d$o.ab, aes(xend=xend, yend=yend)) +
  geom_segment(
    data=d$err.arrows, aes(xend=xend, yend=yend),
    arrow=arrow(type='closed'), color='grey65'
  ) +
  geom_point(
    data=d$o, aes(color=I(pcolor), size=psize),
  ) +
  # geom_point(
  #   data=subset(d$o, ptype %in% c('mid', 'end'), size=size.max/3),
  #   shape=21L, fill=NA
  # ) +
  geom_point(data=d$errors, aes(size=val)) +
  geom_point(data=d$o.m, shape=21L, fill=NA, size=18) +
  facet_wrap(~frame) +
  coord_cartesian(ylim=c(0,1), xlim=c(0,1)) +
  scale_size(
    limits=c(0, size.max), range=c(0,10), guide=FALSE
  ) +
  thm.blnk +
  NULL
p

stop('done plot')
ggsave(
  filename=sprintf('~/Downloads/mesh-anim-5/img-%04d.png', k),
  plot=p,
  width=width/dpi, height=height/dpi, units='in', device='png',
  dpi=dpi
)
}
stop('done plot')

