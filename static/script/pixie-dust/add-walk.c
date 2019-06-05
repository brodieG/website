#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
  Cumulative sum of `x` (should be logical), where TRUE == 1 and FALSE == -1.
 
  CFLAGS = -O0 -Wall -std=gnu99  // in ~/.R/Makevars
  tools::Rcmd('shlib add-walk.c')
  dyn.load('add-walk.so')
  .Call('add_walk', sample(c(TRUE, FALSE), 1e7, rep=TRUE))
 */

SEXP add_walk(SEXP x) {
  int accum = 0;
  R_xlen_t size = XLENGTH(x);
  SEXP res=PROTECT(allocVector(INTSXP, size));
  int *res_ptr=INTEGER(res);
  int *x_ptr=INTEGER(x);

  for(R_xlen_t i = 0; i < size; ++i) {
    if(*x_ptr > 0) {accum++;} else {accum--;}  // BRANCH
    *res_ptr = accum;
    ++res_ptr;
    ++x_ptr;
  }
  UNPROTECT(1);
  return res;
}
