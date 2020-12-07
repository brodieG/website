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
# Given two functions will interpolate `n` points using `fin` for `[0,
# domain[1]]` and `fout` for `[1 - domain[2], 1]`.  If sum `domain` is less than
# one, then the interpolation will be filled in with a Hermite spline matching
# the end and beginning slopes of the functions.
#
# @param n numeric how many points to output.
# @param fin a function that accepts monotonically increasing inputs in [0-1]
#    and maps them into monotonically increasing output in [0-1].
# @param fout like fin, except that it's output will be used reversed to define
#   the "easing out" part of the result.
# @param domain numeric(2) how much of the output domain should be allocated
#    respectively to points derived from `fin` and `fout`.  The sum of the two
#    values should be less than or equal to one, with each value greater than
#    or equal to zero.  If the sum of the two is less than one, the balance of
#    the domain will be used to join the `fin` and `fout` portion of the outputs
#    matching the slopes of the two with a Hermite spline.
# @value numeric `n` values in [0-1] spaced according to the inputs.

ease_in_out <- function(n, fin, fout, domain=c(.5, .5)) {
  vetr(
    INT.1.POS.STR, is.function(.), is.function(.),
    numeric(2) && all_bw(., 0, 1) && sum(.) <= 1
  )
  mid <- 1 - sum(domain)
  nin <- round(n * domain[1])
  nout <- round(n * domain[2])
  nmid <- n - nin - nout

  if((nin < 2 || nout < 2) && nmid > 0) stop("Insufficient points")

  pinxin <- seq(0, 1, length.out=nin)
  pinx <- pinxin * domain[1]
  pin <- fin(pinxin) * domain[1]

  poutxin <- rev(seq(0, 1, length.out=nout))
  poutx <- 1 - poutxin * domain[2]
  pout <- 1 - fout(poutxin) * domain[2]

  pmid <- if(nmid) {
    mxin <- c(pinx[length(pinx)], poutx[1])
    myin <- c(pin[length(pin)], pout[1])
    slope1 <- diff(pin[length(pin) - 1:0]) / diff(pinx[length(pinx) - 1:0])
    slope2 <- diff(pout[1:2]) / diff(poutx[1:2])

    mx <- seq(pinx[length(pinx)], poutx[1], length.out=nmid)
    my <- splinefunH(mxin, myin, c(slope1, slope2))(mx)
    list(x=mx, y=my)
  } else list(x=numeric(), y=numeric())

  list(x=c(pinx, pmid[['x']], poutx), y=c(pin, pmid[['y']], pout))
}
