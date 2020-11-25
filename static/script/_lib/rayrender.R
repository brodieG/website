# Rayrender helper functions

# Given root of file (including dir), create new incremented file.  Files
# are expected to end "[0-9]+\\.png"
#
# next_file('~/Downloads/rlang/img-')

library(vetr)
next_file <- function(x) {
  vetr(character(1L) && grepl("(.*?)(\\d+)\\.png$", .))
  pat <- "(.*?)(\\d+)\\.png$"
  dir <- dirname(x)
  f <- basename(x)
  matches <- regmatches(f, regexec(pat, f))
  root <- matches[[1]][2]
  fl <- list.files(dir, pattern=paste0("^", root, "(\\d+)\\.png$"))
  fnum <-
    if(!length(fl)) 0L
    else max(
      as.integer(vapply(regmatches(fl, regexec(pat, fl)), '[', "", 3))
    )

  vet(INT.1.POS, fnum, stop=TRUE)
  file.path(dir, paste0(root, sprintf("%03d", fnum + 1), ".png", collapse=""))
}
