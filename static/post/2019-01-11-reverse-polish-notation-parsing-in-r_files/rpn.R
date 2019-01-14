
chr_to_name <- function(y)
  lapply(y, function(x) if(is.numeric(x)) x else as.name(x))

rpn <- function(...) {
  L <- chr_to_name(list(...))
  i <- 1
  while(length(L) >= i) {
    if(is.name(L[[i]])) {
      L[[i]] <- as.call(L[i-c(0,2,1)])
      L[i-(1:2)] <- NULL
      i <- i-1
    } else {
      i <- i+1
    }
  }
  L[[1]]
}
