# - Function definitions -------------------------------------------------------

# We try to mimic a package environment chain to avoid objects defined in the
# global env "saving" a bad evaluation.

quote_w <- function(x) {
  caller <- parent.frame()  # See appendix for details
  bquote(with(.(caller), .(substitute(x))))
}
env_w <- new.env(parent=baseenv())
env_w[['quote_w']] <- quote_w
environment(quote_w) <- env_w

bquote_w <- function(x) {
  caller <- parent.frame()
  cmd <- eval(bquote(.(bquote)(.(substitute(x)))), caller)
  bquote(with(.(caller), .(cmd)))
}
env_w[['bquote_w']] <- bquote_w
environment(bquote_w) <- env_w

mask <- function(cmd, dat) {
  if(is.language(cmd) && length(cmd) > 1L) {
    # modify `with` commands that contain a live environment
    if(cmd[[1L]] == as.name('with') && is.environment(cmd[[2L]])) {
      cmd[[2L]] <- list2env(dat, parent=cmd[[2L]])
    }
    # recurse on each sub-command
    cmd[2L:length(cmd)] <- lapply(cmd[2L:length(cmd)], mask, dat)
  }
  cmd
}
env_w[['mask']] <- mask
environment(mask) <- env_w

substitute_w <- function(x) {
  caller <- parent.frame()
  caller2 <- sys.frame(sys.parent(2))   # see appendix for explanation
  cmd <- eval(bquote(substitute(.(substitute(x)),.(caller))))
  cmd <- eval(bquote(.(bquote)(.(cmd))), caller2)
  bquote(with(.(caller2), .(cmd)))
}
env_w[['substitute_w']] <- substitute_w
environment(substitute_w) <- env_w

mean_in_data_w <- function(dat, cmd) {
  cmd <- bquote(mean(.(substitute_w(cmd))))
  cmd <- mask(cmd, dat)
  eval(cmd)
}
environment(mean_in_data_w) <- env_w

# - Basic Tests ----------------------------------------------------------------

l.per.cubic.i <- NA
local({
  mean <- function(...) 42         # decoy
  l.per.cubic.i <- 2.54^3 / 1000
  mean_in_data_w(mtcars, disp * l.per.cubic.i)
})

a <- 2
b <- 4
c <- local({
  b <- 8
  quote_w(a + b)
})
exp <- local({
  b <- 16
  d <- 32
  bquote_w(d + 1 + .(c))
})
exp
res <- eval(exp)
res
2 ^ (which(as.logical(intToBits(as.integer(res)))) - 1L)

e <- 64
f <- local({
  e <- 128
  function(x) {
    e <- 256
    substitute_w(x)
  }
})
f(e)
res <- local({
  e <- 512
  eval(f(e))
})
res
2 ^ (which(as.logical(intToBits(as.integer(res)))) - 1L)

package <- function(f, env) {
  f.name <- as.character(substitute(f))
  environment(f) <- env
  env[[f.name]] <- f
}
ns <- new.env(parent=baseenv())
package(substitute_w, ns)
package(bquote_w, ns)

env1 <- new.env(parent=env_w)
env1[['e']] <- 64
env1[['f']] <- local(
  {
    e <- 128
    function(x) {
      e <- 256
      substitute_w(x)
    }
  },
  env1
)

env2 <- new.env(parent=env1)
env2[['e']] <- 512

env2[['g']] <- evalq(
  function(x) {
    e <- 1024
    res <- f(e + .(substitute_w(x)))
    x <- x * 2
    res
  },
  env2
)
evalq(g(e), env2)
res <- eval(evalq(g(e), env2), new.env(parent=baseenv()))
res
2 ^ (which(as.logical(intToBits(as.integer(res)))) - 1L)

# # an option that allows setting env for more stress testing
# # this doesn't work b/c we need the environment and it's calling
# # environment, and we can't get that easily from just a single environemnt.
# # if we didn't support back-quoting we wouldn't need the second env.
# substitute_w2 <- function(expr, env=sys.frame(sys.parent(2))) {
#   caller <- parent.frame()
#   caller2 <- env
#   cmd <- eval(bquote(substitute(.(substitute(expr)),.(caller2))))
#   cmd <- eval(bquote(.(bquote)(.(cmd))), caller2)
#   bquote(with(.(caller2), .(cmd)))
# }
# env <- local({
#   a <- 16
#   b <- 32
#   res <- (function(a) {
#     b <- 10
#     environment()
#   })(a + .(b))
#   b <- 1000
#   res
# })
# (function(a) {
#   substitute_w2(a, env)
# })(1000)

# - Tidyverse Tests ------------------------------------------------------------
#
# Tests that seemed relevant from 0.4.6.9000

# "can unquote hygienically within captured arg"

fn <- function(df, arg) eval(mask(substitute_w(arg), df))
foo <- "bar";
var <- bquote_w(foo)

identical(fn(mtcars, list(var, .(var))), list(bquote_w(foo), "bar"))
var <- bquote_w(cyl)
identical(fn(mtcars, .(var) > 4), mtcars$cyl > 4)
identical(fn(mtcars, list(var, .(var))), list(bquote_w(cyl), mtcars$cyl))
all.equal(fn(mtcars, list(~var, .(var))), list(~var, mtcars$cyl))
all.equal(
  fn(mtcars, list(~~var, .(bquote_w(var)), .(bquote_w(bquote_w(var))))),
  list(~~var, bquote_w(cyl), bquote_w(var))
)

# test_that("quosures are evaluated recursively" {

foo <- "bar"
identical(eval(bquote_w(foo)), "bar")
identical(eval(bquote_w(.(bquote_w(.(bquote_w(foo)))))), "bar")

# test_that("quosures have lazy semantics",
fn <- function(arg) "unforced"
identical(eval(bquote_w(fn(~stop()))), "unforced")

# test_that("nested quosure thunks rechain properly in the non-data mask", {
bar <- "foo"
quo <- bquote_w(identity(.(bquote_w(toupper(.(bquote_w(identity(bar))))))))
identical(eval(quo), "FOO")

# test_that("nested quosures look in their own env", {

n <- 10
f <- function() {
  n <- 100
  bquote_w(n)
}
quo <- bquote_w(.(f()))
identical(eval(quo), 100)

# test_that("eval_tidy uses quosure environment", {

x <- 10
quo <- local({
  y <- 100
  bquote_w(x + y)
})
identical(eval(quo), 110)

# test_that("looks first in `data`", {
x <- 10
data <- list(x = 100)
identical(eval(mask(bquote_w(x), data)), 100)

# test_that("explicit promise works only one level deep", {

f <- function(x) list(env = environment(), f = g(x))
g <- function(y) substitute_w(y)
out <- f(1 + 2 + 3)
expected_f <- with(out$env, bquote_w(x))
identical(out$f, expected_f)

# - Advanced R -----------------------------------------------------------------

subset2 <- function(data, rows) {
  rows <- substitute_w(rows)
  rows_val <- eval(mask(rows, data))
  stopifnot(is.logical(rows_val))
  data[rows_val, , drop = FALSE]
}
resample <- function(df, n) {
  idx <- sample(nrow(df), n, replace = TRUE)
  df[idx, , drop = FALSE]
}
subsample <- function(df, cond, n = nrow(df)) {
  df <- subset2(df, .(substitute_w(cond)))
  resample(df, n)
}
df <- data.frame(x = c(1, 1, 1, 2, 2), y = 1:5)
subsample(df, x == 1)
