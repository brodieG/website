# Compute Device Size To Fully Fill With Plot
#
# This is not tested extensively and I know very little about messing with
# grid.
#
# @param gtable e.g. as produced by `ggplotGrob(ggplot() ...)`
# @param din device dimension in inches, ideally use defaults to avoid
#   calculation issues with npc units.
# @return numeric(2) containing width and height of device that would be fully
#  occupied by provided gtable when rendered.

gtable_dim <- function(gtable, din=par()[['din']]) {
  gw <- gtable::gtable_width(gtable)
  gh <- gtable::gtable_height(gtable)

  wisnull <- grepl('\\d*null$', as.character(gw$arg1))
  wknown <- Reduce('+', grid::convertX(gw$arg1[!wisnull], 'inches'))
  wnull <- as.numeric(Reduce('+', gw$arg1[wisnull]))

  hisnull <- grepl('\\d*null$', as.character(gh$arg1))
  hknown <- Reduce('+', grid::convertX(gh$arg1[!hisnull], 'inches'))
  hnull <- as.numeric(Reduce('+', gh$arg1[hisnull]))

  null.size <- min(
    c((din[1] - wknown) / wnull,
    (din[2] - hknown) / hnull)
  )
  c(wknown + null.size * wnull, hknown + null.size * hnull)
}
