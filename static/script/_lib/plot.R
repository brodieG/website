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
  # for R > 4.0 only
  gw <- unclass(gtable::gtable_width(gtable))[[1]][[2]]
  gh <- unclass(gtable::gtable_height(gtable))[[1]][[2]]

  wisnull <- grid::unitType(gw) == 'null'
  wknown <- Reduce('+', grid::convertX(gw[!wisnull], 'inches'))
  wnull <- as.numeric(Reduce('+', gw[wisnull]))

  hisnull <- grid::unitType(gh) == 'null'
  hknown <- Reduce('+', grid::convertX(gh[!hisnull], 'inches'))
  hnull <- as.numeric(Reduce('+', gh[hisnull]))

  null.size <- min(
    c((din[1] - wknown) / wnull,
    (din[2] - hknown) / hnull)
  )
  c(wknown + null.size * wnull, hknown + null.size * hnull)
}
