# matrix that has most error on the top left part as that is done last
# it's shown as plotted, and re-ordered for actual use

library(data.table)
map <- matrix(c(
  0, 3, 0,
  3, 1, 2,
  0, 2, 0), 3)[,3:1]
# eltif <- raster::raster("~/Downloads/dem_01.tif")
# eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
# elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

map <- elmat1[1:5, 1:5]

source('../website/static/post/2019-08-23-mesh-reduction-1_files/scripts/rtin2.R')
library(watcher)
coord.vars <- do.call(paste0, expand.grid(c('a','b','c','m','lc','rc'), c('x', 'y')))
id.vars <- c('.id', '.line')
vars <- c(coord.vars, 'errors', 'id', 'i')
xx <- watch(errors_rtin2, vars)(map)
zz.raw <- simplify_data(attr(xx, 'watch.data'))
library(reshape2)

frame.ids <- zz.raw[['.scalar']][['.id']]
# frame.ids <- 1:200
scalar.frames <- zz.raw[['.scalar']][['.id']] %in% frame.ids
zz.raw[[1]] <- lapply(zz.raw[[1]], '[', frame.ids)
zz.raw[-1] <- lapply( # assuming df, not necessarily true
  zz.raw[-1], function(x) subset(x, .id %in% frame.ids)
)

zz.vec <- zz.raw[['.scalar']]
dat <- melt(as.data.frame(zz.vec[c(coord.vars, id.vars)]), id.vars=id.vars)
dat[['variable']] <- as.character(dat[['variable']])
var.chrs <- nchar(dat[['variable']])
dat[['label']] <- substr(dat[['variable']], 1, var.chrs - 1L)
dat[['var']] <- substr(dat[['variable']], var.chrs, var.chrs)
dat <- dcast(dat, .id + .line + label ~ var, value.var='value')
dat[['type']] <- 'Coords'
dat.s <- dat
# dat.s <- subset(dat, id %in% 1:40)
pcolor <- c(
  a='#66c2a5', b='#fc8d62', c='grey65',
  m='#8da0cb', lc='#8da0cb88', rc='#8da0cb88'
)
dat.s1 <- subset(dat.s, label %in% c(letters[1:3], 'm', 'lc', 'rc'))
dat.s1 <- transform(dat.s1, pcolor=pcolor[label])
dat.s2 <- subset(dat.s, label %in% letters[1:3])
dat.s2 <- dat.s2[cumsum(rep(c(3, 1, 1, -2), nrow(dat.s2) / 3)) - 2,]
dat.s3a <- subset(dat.s, label %in% c('lc', 'rc', 'm'))
dat.s3 <- rbind(dat.s3a, transform(dat.s3a, type='Errors'))

# Data for child to parent arrows; surely there is a better way to do this

dat.s4a <- melt(dat.s3a[1:5], id.vars=c('.id', '.line', 'label'))
dat.s4b <- dcast(dat.s4a, .id + .line + variable ~ label)
dat.s4c <- melt(
  dat.s4b,
  id.vars=c('.id', '.line', 'variable', 'm')
)
names(dat.s4c) <- c('.id', '.line', 'coord', 'm', 'type', 'c')
dat.s4d <- melt(dat.s4c, id.vars=c('.id', '.line', 'coord', 'type'))
dat.s4 <- dcast(dat.s4d, .id + .line + type ~ coord + variable)
dat.s4[['type']] <- 'Errors'

# Background triangles

dat.s5a <- subset(dat.s2, .line == 52)
max.id <- max(zz.vec[['.id']])
dat.s5 <- do.call(
  rbind,
  lapply(
    unique(dat.s5a[['.id']]),
    function(x) {
      ids <- which(dat.s5a[['.id']] == x)
      ids.len <- length(ids)
      new.ids <- rep(seq(x, max.id, by=1), each=ids.len)
      res <- dat.s5a[rep_len(ids, length.out=length(new.ids)),,drop=FALSE]
      res[['.id']] <- new.ids
      res[['.id.old']] <- rep(x, length(new.ids))
      res
    }
) )

# background points

dat.s6 <- expand.grid(x=seq_len(nrow(map)) - 1, y=seq_len(ncol(map)) - 1)

code <- deparse(errors_rtin2, control='all')
code[[1]] <- ""
dat.lines <- do.call(
  rbind,
  lapply(
    zz.vec$.id,
    function(i) {
      line <- zz.vec$.line[zz.vec$.id == i]
      data.frame(
        y=(-seq_along(code) + line),
        .id=rep(i, length(code)),
        code=code,
        highlight=ifelse(seq_along(code) == line, 'red', 'black')
      )
}) )
dat.lines[['type']] <- 'Coords'
dat.err <- zz.raw$errors
dat.err[['type']] <- 'Errors'
dat.err[['x']] <- dat.err[['x']] - 1L
dat.err[['y']] <- dat.err[['y']] - 1L

dat.meta <- melt(as.data.frame(zz.vec[c('i', 'id', id.vars)]), id.vars=id.vars)
dat.meta <- as.data.table(dat.meta)[, paste0(variable, ': ', value, collapse=', '), .id]
dat.meta[['type']] <- 'Coords'

dpi <- 72
width <- 600
height <- width
size <- nrow(map)

library(ggplot2)
p <- ggplot(dat.s1, aes(x, y)) +
  geom_point(data=dat.s6, color='grey65', size=.5, shape=3) +
  geom_polygon(
    data=dat.s5, aes(group=.id.old), fill='yellow', alpha=0.3,
    color='yellow', size=0.5
  ) +
  geom_path(data=dat.s2, aes(group=.id), color='white', size=1.5) +
  geom_point(size=8, color=dat.s1$pcolor) +
  geom_segment(
    data=dat.s4, aes(x=x_c, y=y_c, xend=x_m, yend=y_m),
    arrow=arrow(type='closed'), color='grey65'
  ) +
  geom_point(data=dat.err, aes(y=y, x=x, size=val)) +
  geom_point(
    data=dat.s3, fill='NA', color='black', shape=21, size=18
  ) +
  geom_text(
    data=dat.meta,
    aes(y=size - 1 + 1, x=(size - 1) * 1.1, label=V1),
    hjust=0
  ) +
  geom_text(aes(label=label)) +
  geom_text(
    data=dat.lines,
    # aes(x=-2.55, y=y*.1, label=code),
    aes(x=-(nrow(map)-1)/.7843, y=y*.1*(nrow(map)-1)/2, label=code),
    hjust=0, family='mono', color=dat.lines$highlight
  ) +
  guides(color=FALSE) +
  ylab(NULL) + xlab(NULL) +
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank()
  ) +
  labs(title = "Step {frame}/{nframes}") +
  facet_wrap(~type, ncol=1) +
  coord_cartesian(
    ylim=c(1L, nrow(map)) - 1L, xlim=c(1L, nrow(map)) - 1L,
    clip="off"
  ) +
  guides(size=FALSE) +
  theme(
    plot.margin=unit(c(.1, .1, .1, (height/2)/dpi), "inches"),
    panel.grid=element_blank(),
    text=element_text(size=16)
  )
  NULL

# To improve animation:
#
# * Make 'm' same color as the error dots (e.g. black, maybe with white 'm')
# * For emphasis show the 'm' on the error side as well?  Does this confuse
#   error with position semantic?  A little.
# * Stack the panels vertically.
# * Change text to black / highlight.
# * change mx/my to tmpx/tmpy, and Mx/My to mx/my
#

# dev.new(width=width/dpi, height=height/dpi, dpi=dpi)
# p + facet_wrap(~.id)
# stop()
library(gganimate)
p.anim <- p + transition_manual(.id)
# anim_save(
#   '~/Downloads/mesh-anim/anim-1.gif',
#   nframes=length(zz.vec$.id), p.anim, width=width, height=height
# )
res <- animate(
  p.anim,
  nframes = length(zz.vec$.id), device = "png",
  renderer = file_renderer(
    "~/Downloads/mesh-anim-2/", prefix = "gganim-img", overwrite = TRUE
  ),
  width=width, height=height
)

