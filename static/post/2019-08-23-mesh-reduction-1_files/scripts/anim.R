# matrix that has most error on the top left part as that is done last
# it's shown as plotted, and re-ordered for actual use

map <- matrix(c(
  0, 3, 0,
  3, 1, 2,
  0, 2, 0), 3)[,3:1]

source('../website/static/post/2019-08-23-mesh-reduction-1_files/scripts/rtin2.R')
library(watcher)
vars <- c(
  'ax', 'ay', 'bx', 'by', 'cx', 'cy', 'mx', 'my', 'errors',
  'lcx', 'lcy', 'rcx', 'rcy'
)
xx <- watch(errors_rtin2, vars)(map)
zz.raw <- simplify_data(attr(xx, 'watch.data'))
library(reshape2)

# nframes <- 50
# zz <- lapply(zz.raw, head, nframes)
zz.vec <- zz.raw[['.scalar']]
dat <- melt(as.data.frame(zz.vec), id.vars=c('.id', '.line'))
dat[['variable']] <- as.character(dat[['variable']])
var.chrs <- nchar(dat[['variable']])
dat[['label']] <- substr(dat[['variable']], 1, var.chrs - 1L)
dat[['var']] <- substr(dat[['variable']], var.chrs, var.chrs)
dat <- dcast(dat, .id + .line + label ~ var, value.var='value')
dat[['type']] <- '*x-*y'
dat.s <- dat
# dat.s <- subset(dat, id %in% 1:40)

dat.s1 <- subset(dat.s, label %in% c(letters[1:3], 'm'))
dat.s2 <- subset(dat.s, label %in% letters[1:3])
dat.s2 <- dat.s2[cumsum(rep(c(3, 1, 1, -2), nrow(dat.s2) / 3)) - 2,]
dat.s3a <- subset(dat.s, label %in% c('lc', 'rc', 'm'))
dat.s3 <- rbind(dat.s3a, transform(dat.s3a, type='errors'))

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
dat.s4[['type']] <- 'errors'

code <- deparse(errors_rtin2, control='all')
dat.lines <- do.call(
  rbind,
  lapply(
    seq_along(zz.vec$.line),
    function(i) {
      data.frame(
        y=(-seq_along(code) + zz.vec$.line[i]),
        .id=rep(i, length(code)),
        code=code,
        highlight=ifelse(seq_along(code) == zz.vec$.line[i], 'red', 'black')
      )
}) )
dat.lines[['type']] <- '*x-*y'
dat.err <- zz.raw$errors
dat.err[['type']] <- 'errors'
dat.err[['x']] <- dat.err[['x']] - 1L
dat.err[['y']] <- dat.err[['y']] - 1L

dpi <- 72
width <- 900
height <- width/3

library(ggplot2)
p <- ggplot(dat.s1, aes(x, y)) +
  geom_point(
    data=dat.s3, fill='NA', color='black', shape=21, size=18
  ) +
  geom_path(data=dat.s2, aes(group=.id)) +
  geom_point(data=dat.err, aes(y=y, x=x, size=val)) +
  geom_point(aes(color=label), size=8) +
  geom_segment(
    data=dat.s4, aes(x=x_c, y=y_c, xend=x_m, yend=y_m),
    arrow=arrow(type='closed'), color='grey65'
  ) +
  geom_text(aes(label=label)) +
  geom_text(
    data=dat.lines, 
    aes(
      x=-2.5, y=y*.125 + 1.25, label=code, color=I(highlight)
    ),
    hjust=0, family='mono'
  ) +
  guides(color=FALSE) +
  ylab(NULL) + xlab(NULL) +
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank()
  ) +
  labs(title = "ID {frame}") +
  facet_grid(type~.) +
  coord_cartesian(
    ylim=c(1L, nrow(map)) - 1L, xlim=c(1L, nrow(map)) - 1L,
    clip=FALSE
  ) +
  guides(size=FALSE) +
  theme(plot.margin=unit(c(0, 0, 0, (height)/dpi), "inches"))
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
# p + facet_wrap(~id)
library(gganimate)
p.anim <- p + transition_manual(.id)
# anim_save(
#   '~/Downloads/mesh-anim/anim-1.gif',
#   nframes=nrow(zz$.scalar), p.anim, width=width, height=height
# )
res <- animate(
  p.anim,
  nframes = length(zz.vec$.id), device = "png",
  renderer = file_renderer(
    "~/Downloads/mesh-anim/", prefix = "gganim-img", overwrite = TRUE
  ),
  width=width, height=height
)

