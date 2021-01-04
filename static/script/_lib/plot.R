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

# Produce an Easing Path
#
# Eases in with `fin`, and eases out at using a quadratic bezier that slope
# matches to `end`.
#
# @param n numeric how many points to output.
# @param f.in a function that accepts monotonically increasing inputs in [0-1]
#   and maps them into monotonically increasing output in [0-1].
# @end numeric(2) coordinates in ([0-1],[0-1]) to end the portion of the easing
#   curve defined by `fin`.
# @return numeric `n` values in [0-1] spaced according to the inputs.

ease_in_smooth_out <- function(n, f.in, end=c(.5, .5)) {
  vetr(INT.1.POS.STR, is.function(.), numeric(2) && all_bw(., 0, 1))

  xend <- end[1]
  yend <- end[2]

  nin <- round(n * end[1])
  nout <- n - nin

  if(nin < 2 && nout) stop("Insufficient points")

  pinxin <- seq(0, 1, length.out=nin)
  pinx <- pinxin * end[1]
  piny <- f.in(pinxin) * end[2]

  pout <- if(nout) {
    # use a quadratic bezier to connect end point
    last2 <- length(piny) - 1:0
    slopeend <- diff(piny[last2]) / diff(pinx[last2])

    p0 <- end
    p1 <- c(end[1] + (1 - end[2]) / slopeend, 1)
    p2 <- c(1, 1)

    # adapted from (vectorized):
    # https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Quadratic_B%C3%A9zier_curves

    qbez <- function(t, p0, p1, p2) {
      t <- t(t)
      t(p0 %*% (1 - t) + p1 %*% t) * rep((1 - t), 2) +
        t(p1 %*% (1 - t) + p2 %*% t) *  rep(t, 2)
    }
    pout.raw <- qbez(seq(0, 1, length.out=nout * 5), p0, p1, p2)

    # Approximate to regular intervals along x
    poutx <- seq(end[1], 1, length.out=nout + 1)[-1]
    approx(pout.raw, xout=poutx)
  } else list(x=numeric(), y=numeric())

  list(x=c(pinx, pout[['x']]), y=c(piny, pout[['y']]))
}
