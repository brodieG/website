# Horrible horrible, but it works

set.seed(1)
gn <- 10
g <- sample(seq_len(gn), 95, replace=TRUE)
levels <- c('start', 'order', 'longest', 'allocate', 'embed', 'colSums')

# labels are:
# 0: original colored squares
# 1: first set of re-allocation (first column)
# 3: rest of reallocation
# 4: background for colored squares

dat0 <- data.frame(
  step=1L, id=seq_along(g), g,
  x=(seq_along(g) - 1) %/% 10, y=-((seq_along(g) - 1) %% 10),
  alpha=1, label=0
)
dat10 <- rbind(transform(dat0, label=4, g=NA), dat0)
dat20 <- transform(dat10, step=2L)
dat20 <- rbind(
  subset(dat20, label==4),
  transform(
    subset(dat20, label==0)[order(g),],
    x=(seq_along(g) - 1) %/% 10, y=-((seq_along(g) - 1) %% 10)
  )
)
g.rle <- with(subset(dat20, label==0), rle(g))

dat30 <- transform(
  dat20, step=3L,
  alpha=ifelse(
    g==g.rle[['values']][which.max(g.rle[['lengths']])], 1, .2
  )
)
g.max <- nrow(subset(dat30, alpha==1))
dat30 <- rbind(
  dat30,
  transform(subset(dat30, alpha==1), g=NA, label=1)
)
dat30 <- rbind(
  subset(dat30, label%in%c(0,4)),
  transform(
    subset(dat30, label==1), x=gn + 1, y=-(seq_along(y) - 1)
  )
)
dat30 <- do.call(
  rbind,
  c(
    list(dat30),
    replicate(
      gn - 1L, transform(subset(dat30, label==1), label=3), simplify=FALSE)
) )
dat30[dat30[['label']] == 3, 'id'] <- seq_along(which(dat30[['label']] == 3))
dat50 <- transform(
  dat30,
  step=4L,
  alpha=1
)
dat50[dat50[['label']] %in% c(1,3),] <- transform(
  subset(dat50, label %in% c(1,3)),
  x=rep((seq_len(gn)) + gn, each=g.max)
)
dat60 <- transform(
  rbind(
    subset(dat50, label %in% c(1,3,4)),
    transform(
      subset(dat50, label==0), x=gn + g, y=-sequence(g.rle[['lengths']]) + 1
    )
  ),
  step=5L
)
dat70 <- transform(
  rbind(
    subset(dat60, label %in% c(1, 3, 4)),
    transform(subset(dat60, label == 0), y=0)
  ),
  step=6L
)
steps <- c(
  "Start",
  "Order by Group",
  "Compute Length of Longest Group",
  "Alloc Matrix w/ Enough Rows to Fit Longest Group in One Col",
  "Copy Each Group Into a Col",
  "Compute colSums"
)
dat <- transform(
  rbind(dat10, dat20, dat30, dat50, dat60, dat70),
  step=factor(steps[step], levels=steps)
)
# Code to line up with the steps

code <- c(
'set.seed(1)
g <- sample(10, 95, r=TRUE) # groups
x <- runif(95)              # values',

'o <- order(g)
go <- g[o]
xo <- x[o]',

"g.rle <- rle(go)
g.lens <- g.rle[['lengths']]
g.max <- max(g.lens)",

"res <- matrix(
  0, ncol=length(g.lens), nrow=g.max
)",

"pad <- g.max - g.lens + 1L
id.raw <- rep(1L, length(xo))
id.raw[(cumsum(g.lens) + 1L)] <- pad
id <- cumsum(head(id.raw, -1L))
res[id] <- xo",

"colSums(res)"
)
dat.code <- data.frame(text=code, step=factor(steps))
dat <- dat[with(dat, order(step, -label)),]

# dat <- subset(dat, step==steps[[5]])
# dat.code <- subset(dat.code, step==steps[[5]])

library(ggplot2)
library(gganimate)

make_plot <- function(dat, dat.code) {
  ggplot(dat) +
    geom_tile(
      aes(x, y, fill=g, alpha=I(alpha), group=paste(label, id)),
      color='#AAAAAA'
    ) +
    geom_text(
      data=dat.code, aes(label=text), x=-0.5, y=-10,
      hjust=0, vjust=1, size=6, family='mono'
    ) +
    scale_fill_viridis_c(guide=FALSE, na.value='#F3F3F3') +
    scale_alpha(guide=FALSE, rescaler=function(x, to, from, ...) x) +
    theme(
      panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
      axis.title.x=element_blank(), axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(), axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.title=element_text(size=16)
    )
}
p <- make_plot(dat, dat.code)
p + facet_wrap(~step)
anim_save(
  '~/Downloads/colsums-anim.gif',
  p +  transition_states(step, wrap=FALSE, state_length=4) +
    labs(title = "{closest_state}"),
  width=800, height=557, end_pause=5, nframes=350, fps=25
)

png.root <- '~/Downloads/colsums/img-%03d.png'

for(i in 1:6) {
  png(sprintf(png.root, i), width=800, height=557)
  dat.sub <- subset(dat, step==steps[[i]])
  dat.code.sub <- subset(dat.code, step==steps[[i]])

  p <- make_plot(dat.sub, dat.code.sub) +
    ggtitle(steps[[i]]) +
    coord_cartesian(xlim=c(0,20), ylim=c(0,-13))
  print(p)
  dev.off()
}

