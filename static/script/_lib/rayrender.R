# Rayrender helper functions

# Given root of file (including dir), create new incremented file.  Files
# are expected to end "[0-9]+\\.png"
#
# next_file('~/Downloads/rlang/img-')

library(vetr)
next_file <- function(x) {
  pat <-  "(\\d+)\\.png$"
  vetr(character(1L) && grepl(., pat))
  dir <- dirname(x)
  f <- basename(x)
  fl <- list.files(dir, full.names=TRUE, pattern=pat)
  root <- sub(pat, "", x)
  fnum <-
    if(!length(fl)) 0L
    else max(as.integer(sub(".*?(\\d+)\\.png", "\\1", fl, perl=TRUE)), 0L)

  vet(INT.1.POS, fnum, stop=TRUE)
  file.path(dir, paste0(root, sprintf("%03d", fnum + 1), ".png", collapse=""))
}
