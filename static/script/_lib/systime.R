
sys.time <- function(exp, reps=11) {
  res <- matrix(0, reps, 5)
  time.call <- quote(system.time({NULL}))
  time.call[[2]][[2]] <- substitute(exp)
  gc()
  for(i in seq_len(reps)) {
    res[i,] <- eval(time.call, parent.frame())
  }
  structure(res[order(res[,3]),], class='proc_time2')
}
print.proc_time2 <- function(x, ...) {
  print(
    structure(
      x[order(x[,3]),][ceiling(nrow(x)/2),],
      names=c("user.self", "sys.self", "elapsed", "user.child", "sys.child"),
      class='proc_time'
) ) }

