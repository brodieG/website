library(watcher)
library(gganimate)
library(ggplot2)

# Generate data

set.seed(1220)
x <- runif(10)
raw <- watch(insert_sort, c('i', 'j', 'x'))(x)
dat.s <- simplify_data(attr(raw, 'watch.data'))
code.txt <- attr(raw, 'watch.code')

# Extract x data and augment with the corresponding scalar `j` loop index

xs <- dat.s$x
xs <- transform(
  xs, j=dat.s[['.scalar']][['j']][.id],
  ix=rep_len(seq_along(x), length(val))
)
xs[['j']][is.na(xs[['j']])] <- 0

# Display settings

dpi <- 72
width <- 600
height <- width / 3 * 2
x.size <- max(xs$ix) + 1
y.max <- max(xs[['val']])

# Plot!

p <- ggplot(xs, aes(x=ix, y=val)) +
  geom_col(aes(fill=I(ifelse(ix==j, 'red', 'grey35')))) +
  geom_label(
    data=dat.s[['.scalar']], aes(x=i, label='i', y=-.08*y.max),
    size=8, label.padding=unit(0.5, "lines"), family='mono'
  ) +
  geom_label(
    data=dat.s[['.scalar']], aes(x=j, label='j', y=-.2*y.max),
    size=8, label.padding=unit(0.5, "lines"), family='mono'
  ) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  coord_cartesian(clip = 'off', xlim=c(0.5,max(xs$ix)+.5)) +
  geom_text(
    data=expand_text(code.txt, dat.s),
    aes(
      label=code, x=-x.size - 4,
      y=y.raw/(length(code.txt)*2.1),
      color=I(ifelse(highlight, 'red', 'grey35')),
      fontface=I(ifelse(highlight, 'bold', 'plain'))
    ), hjust=0, vjust=0.5, size=5, family='mono'
  ) +
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
    plot.margin=unit(c(.1, .1, .1, (width/1.75)/dpi), "inches"),
    panel.grid=element_blank()
  ) +
  NULL

p.anim <- p + transition_manual(.id)

# anim_save(
#   '~/Downloads/sort-2.gif', nframes=length(dat.s$.scalar$.id), p.anim,
#   width=width, height=height, fps=5
# )

