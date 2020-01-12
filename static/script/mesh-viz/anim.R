# - Initialize -----------------------------------------------------------------

# matrix that has most error on the top left part as that is done last
# it's shown as plotted, and re-ordered for actual use

# map <- matrix(c(
#   0, 3, 0,
#   3, 1, 2,
#   0, 2, 0), 3)[,3:1]
if(!'eltif' %in% ls()) {
  eltif <- raster::raster("~/Downloads/dem_01.tif")
  eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
  elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))
}

map <- elmat1[1:5, 1:5]

source('static/script/mesh-viz/rtin2.R')

# - Record ---------------------------------------------------------------------

if(!'errors_watched' %in% ls()) {
  library(watcher)
  coord.vars <- do.call(paste0, expand.grid(c('a','b','c','m','lc','rc'), c('x', 'y')))
  id.vars <- c('.id', '.line')
  vars <- c(coord.vars, 'errors', 'id', 'i')
  errors_watched <- watch(errors_rtin2, vars)
  xx <- errors_watched(map)
  zz.raw <- simplify_data(attr(xx, 'watch.data'))
  size.max <- max(xx, na.rm=TRUE)
  library(reshape2)

  frame.ids <- zz.raw[['.scalar']][['.id']]
  # frame.ids <- tail(frame.ids, 1)
  scalar.frames <- zz.raw[['.scalar']][['.id']] %in% frame.ids
  zz.raw[[1]] <- lapply(zz.raw[[1]], '[', frame.ids)
  zz.raw[-1] <- lapply( # assuming df, not necessarily true
    zz.raw[-1], function(x) subset(x, .id %in% frame.ids)
  )
}
# - Basic Data -----------------------------------------------------------------

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
  a='#66c2a5', b='#66c2a5', c='grey65',
  # a='#66c2a5', b='#fc8d62', c='grey65',
  m='#8da0cb', lc='#fc8d62', rc='#fc8d62'
)
psizes <- c(a=1/6, b=1/6, c=1/6, m=1/4, lc=1/8, rc=1/8) * size.max * 4

dat.s1 <- subset(dat.s, label %in% c(letters[1:3], 'm', 'lc', 'rc'))
dat.s1 <- transform(dat.s1, pcolor=pcolor[label], psize=psizes[label])
dat.s2 <- subset(dat.s, label %in% letters[1:3])
dat.s2 <- dat.s2[cumsum(rep(c(3, 1, 1, -2), nrow(dat.s2) / 3)) - 2,]
dat.s3a <- subset(dat.s, label %in% c('lc', 'rc', 'm'))

# for circles around points, now we don't circle chilren any more

dat.s3 <- rbind(dat.s3a, transform(dat.s3a, type='Errors'))
dat.s3 <- subset(dat.s3, label == 'm')

# Data for child to parent arrows; surely there is a better way to do this

dat.s4a <- melt(dat.s3a[1:5], id.vars=c('.id', '.line', 'label'))
dat.s4b <- dcast(dat.s4a, .id + .line + variable ~ label)
dat.s4c <- melt(
  dat.s4b,
  id.vars=c('.id', '.line', 'variable', 'm')
)
names(dat.s4c) <- c('.id', '.line', 'coord', 'm', 'ctype', 'c')
dat.s4d <- melt(dat.s4c, id.vars=c('.id', '.line', 'coord', 'ctype'))
dat.s4 <- dcast(dat.s4d, .id + .line + ctype ~ coord + variable)
dat.s4[['ctype']] <- NULL

dat.s4 <- transform(dat.s4, diag=ifelse(x_c - x_m & y_c - y_m, sqrt(2), 1))
arrow.mult <- log2(nrow(map) - 1) * 3
dat.s4 <- transform(
  dat.s4,
  x_m=x_m - sign(x_m - x_c) * 1 / (arrow.mult * diag),
  y_m=y_m - sign(y_m - y_c) * 1 / (arrow.mult * diag)
)
# Background triangles

dat.s5a <- subset(dat.s2, .line == 45)
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

# - Code -----------------------------------------------------------------------

code <- deparse(errors_rtin2, control='all')
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

dat.lines <- do.call(rbind, code.rez)
dat.lines[['type']] <- 'Coords'

# - Errors ---------------------------------------------------------------------

dat.err <- zz.raw$errors
dat.err[['type']] <- 'Errors'
dat.err[['x']] <- dat.err[['x']] - 1L
dat.err[['y']] <- dat.err[['y']] - 1L

dat.meta <- melt(as.data.frame(zz.vec[c('i', 'id', id.vars)]), id.vars=id.vars)
dat.meta <- stack(
  lapply(
    split(dat.meta, dat.meta$.id), 
    function(x) with(x, paste0(variable, ': ', value, collapse=', '))
) )
names(dat.meta)[2] <- '.id'
dat.meta[['type']] <- 'Coords'

# - Plot -----------------------------------------------------------------------

dpi <- 72
width <- 600
height <- width
size <- nrow(map)

# ggsave(
#   sprintf('~/Downloads/mesh-anim-2/gganim-img%04d.svg', 1),
#   p, width=width/dpi, height=height/dpi, units='in'
# )
library(ggplot2)
cat('\n')
frames <- sort(unique(dat.s1$.id))
data <- list(
  s1=dat.s1, s5=dat.s5, s2=dat.s2, s4=dat.s4, err=dat.err,
  meta=dat.meta, lines=dat.lines, s3=dat.s3
)
for(i in frames) {
  cat(sprintf("\rFrame %04d", i))
  d <- lapply(data, function(x) subset(x, .id == i))

  p <- ggplot(d$s1, aes(x, y)) +
    geom_point(data=dat.s6, color='grey65', size=.5, shape=3) +
    geom_polygon(
      data=d$s5, aes(group=.id.old), fill='yellow', alpha=0.15,
      color='yellow', size=0.5
    ) +
    geom_path(data=d$s2, aes(group=.id), color='white', size=1.5) +
    geom_point(
      data=d$s3, fill='NA', color='black', shape=21, size=18
    ) +
    geom_segment(
      data=d$s4, aes(x=x_c, y=y_c, xend=x_m, yend=y_m),
      arrow=arrow(type='closed', length=unit(0.25, 'inches')), color='grey65'
    ) +
    geom_point(aes(size=psize), color=d$s1$pcolor) +
    geom_point(data=d$err, aes(y=y, x=x, size=val)) +
    geom_text(aes(label=label)) +
    geom_label(
      data=subset(d$lines, highlight=='white'),
      aes(
        x=-(nrow(map)-1)/.71,
        y=y*(nrow(map)-1)/cwindow*2.5 + (nrow(map)-1) * .035, label=code
      ),
      hjust=0, family='mono', color=NA, fill='grey20', 
      label.padding = unit(0.15, "lines"),
    ) +
    geom_text(
      data=d$lines,
      # aes(x=-2.55, y=y*.1, label=code),
      aes(
        x=-(nrow(map)-1)/.71,
        y=y*(nrow(map)-1)/cwindow*2.5 + (nrow(map)-1) * .035, label=code
      ),
      hjust=0, family='mono', color=d$lines$highlight
    ) +
    guides(color=FALSE) +
    ylab(NULL) + xlab(NULL) +
    labs(title = "Step {frame}/{nframes}") +
    facet_wrap(~type, ncol=1) +
    coord_fixed(
      ylim=c(1L, nrow(map)) - 1L, xlim=c(1L, nrow(map)) - 1L,
      clip="off"
    ) +
    guides(size=FALSE) +
    theme(
      axis.text.x=element_blank(), axis.text.y=element_blank(),
      axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
      plot.margin=unit(c(.1, .1, .1, (height/2)/dpi), "inches"),
      panel.grid=element_blank(),
      text=element_text(size=16)
    ) +
    ggtitle(sprintf('Frame %04d/%d (%s)', i, max(frames), d$meta$values)) +
    scale_size(limits=c(0, max(xx, na.rm=TRUE)), range=c(0,10)) +
    NULL
  ggsave(
    filename=sprintf('~/Downloads/mesh-anim-4d/img-%04d.png', i),
    plot=p,
    width=width/dpi, height=height/dpi, units='in', device='png',
    dpi=dpi
  )
}
stop('done plot')

# remove transparency layers, doesn't work but at least this way consistent
# profile

dir <- '~/Downloads/mesh-anim-4d'
files <- list.files(dir, full.names=TRUE, pattern='img-.*\\.png')
for(i in files) {
  fpng <- png::readPNG(i)[,,1:3]
  png::writePNG(fpng, i)
}
# Add frames at end, 2 second pause at 60fps

new.files <- paste0(dir, sprintf('/img-%04d.png', length(files) + 1:120))
for(i in new.files) file.copy(tail(files, 1), i)


# ffmpeg -framerate 10 -pattern_type glob -i '*.png' -pix_fmt yuv420p out.mp4 &&
#   open out.mp4
#
# ffmpeg -framerate 30 -pattern_type glob -i '*.png' -pix_fmt yuv420p out.mp4 &&
#   open out.mp4
#
# ffmpeg -framerate 30 -pattern_type glob -i '*.png' out.mp4 &&
#   open out.mp4

# Trying to mess with colors, can't seem to get the right profile.  This seems
# like the most relevant hit: https://bugzilla.mozilla.org/show_bug.cgi?id=1300170
# ffmpeg -framerate 30 -pattern_type glob -i '*.png' -pix_fmt yuv420p \
#   -color_primaries bt709 -color_trc bt709 -colorspace bt709 out.mp4 

# ffmpeg -framerate 30 -pattern_type glob -i '*.png' -pix_fmt yuv420p out2.mp4 

# Things to do:
# * A play button in marquee
# * Some cue that the flipbook finished and is restarting

# - Combine Frames -------------------------------------------------------------

# Show frames 100, 194, 382, 758

filenums <- c(100, 194, 382, 758)
files <- paste0(dir, '/img-', sprintf('%04d.png', filenums))

pngs <- lapply(lapply(files, png::readPNG), '[', , 311:586,)
pngs[1:3] <- lapply(pngs[1:3], '[', ,1:266,)
pngcols <- vapply(pngs, ncol, 0)

pngres <- array(numeric(), c(600, sum(pngcols), 3))
pngres[,1:266,] <- pngs[[1]]
pngres[,1:266 + 266,] <- pngs[[2]]
pngres[,1:266 + 266*2,] <- pngs[[3]]
pngres[,1:276 + 266*3,] <- pngs[[4]]

png::writePNG(pngres, '~/Downloads/mesh-anim-4-abreast.png')

# two abreast

filenums <- c(1102)
dir <- '~/Downloads/mesh-anim-4d'
files <- paste0(dir, '/img-', sprintf('%04d.png', filenums))
png1 <- png::readPNG(files[1])[,311:586,]
rle(rowSums(png1[,200,]) == 3)
newheight <- 313
pngres <- array(1, c(newheight, 276 + 266, 3))
pngres[,1:266,] <- png1[1:newheight, 1:266,]
offset <- newheight - 277 - 5
pngres[offset:newheight,1:276 + 266,] <- 
  png1[newheight:(600-5), 1:276,]

# anti-aliasing messes things up, so we have to add the anti-aliased row to
# the second panel

rle(rowSums(pngres[40,,1:3]) == 3)
pngres[31, 276+1:256,] <- pngres[31, 10+1:256,]
png::writePNG(pngres, '~/Downloads/mesh-anim-2-abreast.png')

plot(as.raster(pngres))

dir1 <- '~/Downloads/mesh-anim-4d'
dir2 <- '~/Downloads/mesh-anim-4d-1'
files <- list.files(dir1, pattern='img-.*\\.png')
# files <- files[1:50]

for(i in files) {
  png1 <- png::readPNG(sprintf('%s/%s', dir1, i))[,311:586,]
  newheight <- 313
  pngres <- array(1, c(newheight + 1, 276 + 266, 3))
  pngres[1:newheight,1:266,] <- png1[1:newheight, 1:266,]
  offset <- newheight - 277 - 5
  pngres[offset:newheight,1:276 + 266,] <- png1[newheight:(600-5), 1:276,]

  pngres[31, 276+1:256,] <- pngres[31, 10+1:256,]
  png::writePNG(pngres, sprintf('%s/%s', dir2, i))
}


# anti-aliasing messes things up, so we have to add the anti-aliased row to
# the second panel

rle(rowSums(pngres[40,,1:3]) == 3)

plot(as.raster(pngres))

# Two abreast all
