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
ptypes <- c('mid', 'end', 'end', rep('child', 5))
o <- transform(o, ptype=ptypes[(i - 1) %/% els + 1])
data <- list(o=o, errors=errors)

frames <- 15:50
library(ggplot2)
for(k in frames) {
  cat(sprintf("\rFrame %04d", k))
  d <- lapply(data, function(x) subset(x, .id == k))

  p <- ggplot(mapping=aes(x, y)) +
    geom_point(data=d$o, aes(color=as.character(ptype))) +
    geom_point(data=d$errors, aes(size=val)) +
    facet_wrap(~frame) +
    coord_cartesian(ylim=c(0,1), xlim=c(0,1)) +
    thm.blnk +
    NULL
  ggsave(
    filename=sprintf('~/Downloads/mesh-anim-5/img-%04d.png', k),
    plot=p,
    width=width/dpi, height=height/dpi, units='in', device='png',
    dpi=dpi
  )
}
stop('done plot')
