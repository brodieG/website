source('../website/static/post/2019-08-23-mesh-reduction-1_files/scripts/rtin2.R')
library(watcher)
vars <- c('ax', 'ay', 'bx', 'by', 'cx', 'cy', 'mx', 'my', 'errors')
xx <- watch(errors_rtin2, vars)(map)
zz.raw <- simplify_data(attr(xx, 'watch.data'))
library(reshape2)

# nframes <- 50
# zz <- lapply(zz.raw, head, nframes)
zz.vec <- zz.raw[['.scalar']]
dat <- melt(as.data.frame(zz.vec), id.vars=c('.id', '.line'))
dat[['label']] <- substr(dat[['variable']], 1, 1)
dat[['var']] <- substr(dat[['variable']], 2, 2)
dat <- dcast(dat, .id + .line + label ~ var, value.var='value')
dat[['type']] <- 'a'
# dat.s <- subset(dat, id %in% 1:40)

dat.s <- dat
dat.s2 <- subset(dat.s, label %in% letters[1:3])
dat.s2 <- dat.s2[cumsum(rep(c(3, 1, 1, -2), nrow(dat.s2) / 3)) - 2,]

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
        highlight=seq_along(code) == zz.vec$.line[i]
      )
}) )
dat.lines[['type']] <- 'a'
dat.err <- zz.raw$errors
dat.err[['type']] <- 'c'
dat.err[['x']] <- dat.err[['x']] - 1L
dat.err[['y']] <- dat.err[['y']] - 1L

# dat.s <- subset(dat.s, .id == 40)
# dat.s2 <- subset(dat.s2, .id == 40)
# dat.err <- subset(dat.err, .id == 40)
# dat.lines <- subset(dat.lines, .id == 40)

dpi <- 72
width <- 900
height <- width/3

library(ggplot2)
p <- ggplot(dat.s, aes(x, y)) +
  geom_path(data=dat.s2, aes(group=.id)) +
  geom_point(data=dat.err, aes(y=y, x=x, size=val)) +
  geom_point(aes(color=label), size=8) +
  geom_text(aes(label=label)) +
  geom_text(
    data=dat.lines, 
    aes(
      x=-2.5, y=y*.125 + 1.25, label=code, color=highlight
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
  facet_wrap(~type) +
  coord_cartesian(
    ylim=c(1L, nrow(map)) - 1L, xlim=c(1L, nrow(map)) - 1L,
    clip=FALSE
  ) +
  guides(size=FALSE) +
  theme(plot.margin=unit(c(0, 0, 0, height/dpi), "inches"))
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
animate(
  p.anim,
  nframes = length(zz.vec$.id) + 1L, device = "png",
  renderer = file_renderer(
    "~/Downloads/mesh-anim/", prefix = "gganim-img", overwrite = TRUE
  ),
  width=width, height=height
)

