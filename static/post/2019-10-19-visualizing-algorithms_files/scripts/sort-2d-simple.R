library(watcher)
library(gganimate)
library(reshape2)

# Generate data

set.seed(1220)
x <- runif(10)
raw <- watch(insert_sort, c('i', 'j', 'x'))(x)
watch.simple <- simplify_data(attr(raw, 'watch.data'))

# Extract x data and augment with the corresponding scalar `j` loop index

xs <- watch.simple[['x']]
xs <- transform(
  xs, j=watch.simple[['.scalar']][['j']][.id], ix=rep_len(seq_along(x), length(val))
)
xs[['j']][is.na(xs[['j']])] <- 0

# Data for labels

labs <- melt(watch.simple[['.scalar']][c('.id', 'i', 'j')], id.var='.id')

# Plot!

library(ggplot2)
p <- ggplot(xs, aes(x=ix, y=val)) +
  geom_col(aes(fill=I(ifelse(ix==j, 'red', 'grey35')))) +
  geom_label(
    data=labs, aes(x=value, label=variable, y=-0.25 + (variable=='j') * .125),
    size=8, label.padding=unit(0.5, "lines"), family='mono'
  ) +
  gganimate::transition_manual(.id) +
  NULL

gganimate::anim_save(
  animation=p, '~/Downloads/sort-3.gif', nframes=nrow(watch.simple[['.scalar']]),
  width=400, height=400, fps=5
)


   +
  geom_label(
    data=dat[['.scalar']], aes(x=j, label='j', y=-.2*y.max),
    size=8, label.padding=unit(0.5, "lines"), family='mono'
  ) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  coord_cartesian(clip = 'off', xlim=c(0.5,max(xs$ix)+.5)) +
  geom_text(
    data=expand_text(code.txt, dat),
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


