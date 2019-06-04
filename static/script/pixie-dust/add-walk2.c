#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
  Cumulative sum of `x` (should be logical), where TRUE == 1 and FALSE == -1.

  // Makevars options

  CFLAGS += -S -masm=intel -O0   // disassembly
  CFLAGS = -O0 -Wall -std=gnu99  // in ~/.R/Makevars

  tools::Rcmd('shlib add-walk2.c')
  dyn.load('add-walk2.so')
  .Call('add_walk2', sample(c(TRUE, FALSE), 1e7, rep=TRUE))
 */

SEXP add_walk2(SEXP x) {
  R_xlen_t size = XLENGTH(x);
  int *x_ptr=INTEGER(x);
  int accum = 0;

  for(R_xlen_t i = 0; i < size; ++i) {
    if(*x_ptr > 0) {accum++;} else {accum--;}  // BRANCH
    ++x_ptr;
  }
  return ScalarInteger(accum);
}
