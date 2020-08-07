
basedir <- '~/Downloads/rlang/video-all'
setwd(basedir)
x <- list.files(pattern="img.*\\.png")

frames.fade.f.w <- 15
frames.still <- 30 - frames.fade.f.w

file.copy(
  x,
  file.path(
    basedir, 'final',
    sprintf('img-%03d.png', seq_along(x) + frames.fade.f.w + frames.still)
  )
)
first.frame <- list.files(file.path(basedir, 'final'), full.names=TRUE)[1]
fade.vals <- ((seq_len(frames.fade.f.w) - 1) / (frames.fade.f.w - 1))^2.2
lapply(
  seq_along(fade.vals),
  function(i) {
    png <- png::readPNG(first.frame)
    png::writePNG(
      png * fade.vals[i] + (1 - fade.vals[i]),
      file.path(basedir, 'final', sprintf('img-%03d.png', i))
    )
  }
)
file.copy(
  first.frame,
  file.path(
    basedir, 'final',
    sprintf(
      'img-%03d.png', seq(frames.fade.f.w + 1, length.out=frames.still, by=1L)
  ) )
)





