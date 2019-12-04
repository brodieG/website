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
  wunits <- attr(gw$arg1, 'unit')
  wknown <- Reduce('+', grid::convertX(gw$arg1[wunits != 'null'], 'inches'))
  wnull <- Reduce('+', gw$arg1[wunits == 'null'])

  hunits <- attr(gh$arg1, 'unit')
  hknown <- Reduce('+', grid::convertX(gh$arg1[hunits != 'null'], 'inches'))
  hnull <- Reduce('+', gh$arg1[hunits == 'null'])

  null.size <- min(c((din[1] - wknown) / wnull, (din[2] - hknown) / hnull))
  c(wknown + null.size * wnull, hknown + null.size * hnull)
}
