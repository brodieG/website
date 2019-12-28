source('static/script/mesh-viz/rtin-vec2.R')
source('static/script/mesh-viz/rtin-vec.R')
source('static/script/mesh-viz/extract-vec.R')
source('static/script/mesh-viz/viz-lib.R')

library(watcher)
vars <- c('o', 'o.m', 'o.a', 'o.b',  'o.len', 'errors', 'err.ids')
errors_watched <- watch(compute_error3b, vars)
m <- map[1:5, 1:5]
xx <- errors_watched(m)
zz.raw <- simplify_data(attr(xx, 'watch.data'))

library(watcher)
library(reshape2)
id.vars <- c('.id', '.line')
zz.vec <- zz.raw[['.scalar']]
dat <- melt(as.data.frame(zz.vec[c(vars, id.vars)]), id.vars=id.vars)

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

dpi <- 72
width <- 600
height <- width
size <- nrow(map)

errors <- transform(
  zz.raw$errors, x=(x - 1) / ncol(m), y=(y - 1) / nrow(m),
  frame='Errors'
)
o <- transform(ids_to_df(zz.raw$o, m), frame='Ids')

data <- list(o=o, errors=errors)

frames <- 1:10
for(i in frames) {
  cat(sprintf("\rFrame %04d", i))
  d <- lapply(data, function(x) subset(x, .id == i))

  p <- ggplot(mapping=aes(x, y)) +
    geom_point(data=d$o) +
    geom_point(data=d$errors, aes(size=val)) +
    facet_wrap(~frame) +
    NULL
  ggsave(
    filename=sprintf('~/Downloads/mesh-anim-5/img-%04d.png', i),
    plot=p,
    width=width/dpi, height=height/dpi, units='in', device='png',
    dpi=dpi
  )
}
stop('done plot')
