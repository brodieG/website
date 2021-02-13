#include <R.h>
#include <Rinternals.h>
#include <R_ext/Itermacros.h>

// we only use map for its dimensions

// Outer loop is water, coerce to double at calculation time

SEXP BG_calc_depth(SEXP map, SEXP shorei, SEXP wateri) {
  SEXP mdim = PROTECT(getAttrib(map, R_DimSymbol));
  SEXP sdim = PROTECT(getAttrib(shorei, R_DimSymbol));
  SEXP wdim = PROTECT(getAttrib(wateri, R_DimSymbol));

  // assume dims are INTEGER
  if(XLENGTH(mdim) != 2 || TYPEOF(map) != REALSXP)
    error("`map` should be matrix");
  if(XLENGTH(sdim) != 2 || TYPEOF(shorei) != INTSXP  || INTEGER(sdim)[1] != 2)
    error("`shorei` should be n x 2 numeric matrix");
  if(XLENGTH(wdim) != 2 || TYPEOF(wateri) != INTSXP  || INTEGER(wdim)[1] != 2)
    error("`wateri` should be n x 2 numeric matrix");

  SEXP m = PROTECT(allocMatrix(REALSXP, INTEGER(mdim)[0], INTEGER(mdim)[1]));

  double *mrs, *mr;
  mrs = mr = REAL(m);
  double *mre = REAL(m) + (XLENGTH(m) - 1);
  for(; mr <= mre; ++mr)  *mr = R_PosInf;
  mr = mrs;

  // Assume constant conversion to double is fast rather than convert inputs to
  // double?

  int sn = INTEGER(sdim)[0];
  int wn = INTEGER(wdim)[0];

  int *sx = INTEGER(shorei);
  int *sy = INTEGER(shorei) + sn;
  int *wx = INTEGER(wateri);
  int *wy = INTEGER(wateri) + wn;

  int mrow = INTEGER(mdim)[0];

  for(R_len_t w = 0; w < wn; ++w) {
    double *mp = mr + ((wx[w] - 1) + (wy[w] - 1) * mrow);
    for(R_len_t s = 0; s < sn; ++s) {
      double dx = (sx[s] - wx[w]);
      double dy = (sy[s] - wy[w]);
      double d = dx * dx + dy * dy;
      if(*mp > d) *mp = d;
    }
  }
  UNPROTECT(4);
  return m;
}
// Outer loop is shore

SEXP BG_calc_depth2(SEXP map, SEXP shorei, SEXP wateri) {
  SEXP mdim = PROTECT(getAttrib(map, R_DimSymbol));
  SEXP sdim = PROTECT(getAttrib(shorei, R_DimSymbol));
  SEXP wdim = PROTECT(getAttrib(wateri, R_DimSymbol));

  // assume dims are INTEGER
  if(XLENGTH(mdim) != 2 || TYPEOF(map) != REALSXP)
    error("`map` should be matrix");
  if(XLENGTH(sdim) != 2 || TYPEOF(shorei) != INTSXP  || INTEGER(sdim)[1] != 2)
    error("`shorei` should be n x 2 numeric matrix");
  if(XLENGTH(wdim) != 2 || TYPEOF(wateri) != INTSXP  || INTEGER(wdim)[1] != 2)
    error("`wateri` should be n x 2 numeric matrix");

  SEXP m = PROTECT(allocMatrix(REALSXP, INTEGER(mdim)[0], INTEGER(mdim)[1]));

  double *mrs, *mr;
  mrs = mr = REAL(m);
  double *mre = REAL(m) + (XLENGTH(m) - 1);
  for(; mr <= mre; ++mr)  *mr = R_PosInf;
  mr = mrs;

  // Assume constant conversion to double is fast rather than convert inputs to
  // double?

  int sn = INTEGER(sdim)[0];
  int wn = INTEGER(wdim)[0];

  int *sx = INTEGER(shorei);
  int *sy = INTEGER(shorei) + sn;
  int *wx = INTEGER(wateri);
  int *wy = INTEGER(wateri) + wn;

  int mrow = INTEGER(mdim)[0];

  for(R_len_t s = 0; s < sn; ++s) {
    for(R_len_t w = 0; w < wn; ++w) {
      double *mp = mr + ((wx[w] - 1) + (wy[w] - 1) * mrow);
      double dx = (sx[s] - wx[w]);
      double dy = (sy[s] - wy[w]);
      double d = dx * dx + dy * dy;
      if(*mp > d) *mp = d;
    }
  }
  UNPROTECT(4);
  return m;
}
// To figure out the cost of having to use an interim
// buffer before computing shortest distance

SEXP BG_calc_depth3(SEXP map, SEXP shorei, SEXP wateri) {
  SEXP mdim = PROTECT(getAttrib(map, R_DimSymbol));
  SEXP sdim = PROTECT(getAttrib(shorei, R_DimSymbol));
  SEXP wdim = PROTECT(getAttrib(wateri, R_DimSymbol));

  // assume dims are INTEGER
  if(XLENGTH(mdim) != 2 || TYPEOF(map) != REALSXP)
    error("`map` should be matrix");
  if(XLENGTH(sdim) != 2 || TYPEOF(shorei) != INTSXP  || INTEGER(sdim)[1] != 2)
    error("`shorei` should be n x 2 numeric matrix");
  if(XLENGTH(wdim) != 2 || TYPEOF(wateri) != INTSXP  || INTEGER(wdim)[1] != 2)
    error("`wateri` should be n x 2 numeric matrix");

  SEXP m = PROTECT(allocMatrix(REALSXP, INTEGER(mdim)[0], INTEGER(mdim)[1]));

  double *mrs, *mr;
  mrs = mr = REAL(m);
  double *mre = REAL(m) + (XLENGTH(m) - 1);
  for(; mr <= mre; ++mr)  *mr = R_PosInf;
  mr = mrs;

  // Assume constant conversion to double is fast rather than convert inputs to
  // double?

  int sn = INTEGER(sdim)[0];
  int wn = INTEGER(wdim)[0];

  int *sx = INTEGER(shorei);
  int *sy = INTEGER(shorei) + sn;
  int *wx = INTEGER(wateri);
  int *wy = INTEGER(wateri) + wn;

  int mrow = INTEGER(mdim)[0];
  SEXP d = PROTECT(allocVector(REALSXP, wn));

  for(R_len_t s = 0; s < sn; ++s) {
    double *dp = REAL(d);
    for(R_len_t w = 0; w < wn; ++w) {
      double dx = (sx[s] - wx[w]);
      double dy = (sy[s] - wy[w]);
      dp[w] = dx * dx + dy * dy;
    }
    for(R_len_t w = 0; w < wn; ++w) {
      double *mp = mr + ((wx[w] - 1) + (wy[w] - 1) * mrow);
      if(*mp > dp[w]) *mp = dp[w];
    }
  }
  UNPROTECT(5);
  return m;
}
// Timing adding a scaoarl

SEXP BG_add_scalar(SEXP x, SEXP y) {
  if(TYPEOF(x) != REALSXP || TYPEOF(y) != REALSXP)
    error("Bad types");
  if(XLENGTH(y) < 1 || XLENGTH(x) < 1)
    error("Bad length");
  double * xp = REAL(x);
  double * xe = xp + (XLENGTH(x) - 1);
  double add = REAL(y)[0];

  SEXP res = PROTECT(allocVector(REALSXP, XLENGTH(x)));
  double * resp = REAL(res);

  for(; xp <= xe; ++xp, ++resp) *resp = *xp + add;
  UNPROTECT(1);
  return res;
}

// Test rowsums

SEXP BG_row_sums(SEXP x) {
  Rprintf("double %d long %d\n", sizeof(double), sizeof(long double));
  return R_NilValue;
}


SEXP BG_double(SEXP x) {
  R_len_t len = XLENGTH(x);
  double *p = REAL(x);

  for(R_xlen_t i = 0; i < len; ++i) {
    p[i] += p[i];
  }
  return x;
}
// Simple add

SEXP BG_add(SEXP x, SEXP y) {
  if(XLENGTH(x) != XLENGTH(y)) error("bad lengths");
  if(TYPEOF(x) != REALSXP | TYPEOF(x) != TYPEOF(y)) error("bad types");
  R_len_t len = XLENGTH(x);
  double *p = REAL(x);
  double *q = REAL(y);
  SEXP res = PROTECT(allocVector(REALSXP, len));
  double *s = REAL(res);

  for(R_xlen_t i = 0; i < len; ++i) {
    s[i] = p[i] + q[i];
  }
  UNPROTECT(1);
  return res;
}
SEXP BG_sum(SEXP x) {
  if(TYPEOF(x) != REALSXP) error("bad types");
  R_len_t len = XLENGTH(x);
  long double res = 0;
  double *p = REAL(x);
  for(R_xlen_t i = 0; i < len; ++i) {
    res += p[i];
  }
  return ScalarReal((double)res);
}
SEXP BG_mult_add(SEXP x, SEXP y, SEXP z, SEXP w) {
  if(XLENGTH(x) != XLENGTH(y)) error("bad lengths");
  if(TYPEOF(x) != REALSXP | TYPEOF(x) != TYPEOF(y)) error("bad types");
  R_len_t len = XLENGTH(x);
  double *p = REAL(x);
  double *q = REAL(y);
  double *r = REAL(z);
  double *s = REAL(w);
  SEXP res = PROTECT(allocVector(REALSXP, len));
  double *a = REAL(res);

  for(R_xlen_t i = 0; i < len; ++i) {
    a[i] = p[i] * q[i] + r[i] * s[i];
  }
  UNPROTECT(1);
  return res;
}
SEXP BG_mult_add2(SEXP x, SEXP y, SEXP z, SEXP w) {
  if(XLENGTH(x) != XLENGTH(y)) error("bad lengths");
  if(TYPEOF(x) != REALSXP | TYPEOF(x) != TYPEOF(y)) error("bad types");
  R_len_t len = XLENGTH(x);
  double *p = REAL(x);
  double *q = REAL(y);
  double *r = REAL(z);
  double *s = REAL(w);
  SEXP tmp = PROTECT(allocVector(REALSXP, len));
  double *t = REAL(tmp);
  SEXP res = PROTECT(allocVector(REALSXP, len));
  double *a = REAL(res);

  for(R_xlen_t i = 0; i < len; ++i) {
    t[i] = p[i] * q[i];
  }
  for(R_xlen_t i = 0; i < len; ++i) {
    a[i] = r[i] * s[i];
  }
  for(R_xlen_t i = 0; i < len; ++i) {
    a[i] += t[i];
  }
  for(R_xlen_t i = 0; i < len; ++i) {
    a[i] += t[i];
  }
  UNPROTECT(2);
  return res;
}

// Test iteration check

#define NINTERRUPT 10000000

SEXP BG_double_iter(SEXP x) {
  R_len_t len = XLENGTH(x);
  double *p = REAL(x);

  R_xlen_t i = 0;
  R_ITERATE_CHECK(NINTERRUPT, len, i, p[i] += p[i];);
  return x;
}

// rowsums test

SEXP BG_rowsums(SEXP x) {
  if(TYPEOF(x) != REALSXP) error("bad types");
  SEXP dim = PROTECT(getAttrib(x, R_DimSymbol));
  if(XLENGTH(dim) != 2) error("bad dims");

  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];

  SEXP res = PROTECT(allocVector(REALSXP, rows));
  double *q = REAL(res);
  double *p = REAL(x);

  for(int i = 0; i < rows; ++i) {
    long double tmp = *p;
    double *s = p;
    for(int j = 0; j < cols - 1; ++j) {
      s += rows;
      tmp += *s;
    }
    *q = (double)tmp;
    ++q;
    ++p;
  }
  UNPROTECT(2);
  return res;
}
// Same as rowSums, but with doubles

SEXP BG_rowsums2(SEXP x) {
  if(TYPEOF(x) != REALSXP) error("bad types");
  SEXP dim = PROTECT(getAttrib(x, R_DimSymbol));
  if(XLENGTH(dim) != 2) error("bad dims");

  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];

  SEXP res = PROTECT(allocVector(REALSXP, rows));
  double *q = REAL(res);
  double *p = REAL(x);

  for(int i = 0; i < cols; ++i) {
    double *s = q;
    for(int j = 0; j < rows; ++j, ++s, ++p) {
      *s += *p;
    }
  }
  UNPROTECT(2);
  return res;
}
// Same as rowSums, but only 16KB at a time, although ultlimately this probasbly
// doesn't help that much as 16KB is a decently large chunk for a matrix.

SEXP BG_rowsums4(SEXP x) {
  if(TYPEOF(x) != REALSXP) error("bad types");
  SEXP dim = PROTECT(getAttrib(x, R_DimSymbol));
  if(XLENGTH(dim) != 2) error("bad dims");

  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];

  SEXP res = PROTECT(allocVector(REALSXP, rows));
  long double *rans;
  rans = Calloc(rows, long double);

  double *p = REAL(x);
  int block = 16384 / 8;
  int blocks = rows / block + (rows % block != 0);

  for(int k = 0; k < blocks; ++k) {
    Rprintf("K %d\n", k);
    for(int i = 0; i < cols; ++i) {
      int this_block = k * block;
      int next_block = this_block + block;
      int end = rows < next_block ? rows : next_block;
      long double *s = rans + this_block;
      double *q = p + this_block + i * rows;
      for(int j = this_block; j < end; ++j) {
        *s++ += *q++;
      }
    }
  }
  long double *s = rans;
  double *q = REAL(res);
  for(int i = 0; i < rows; ++i) {
    *q++ = (double)*s++;
  }
  Free(rans);
  UNPROTECT(2);
  return res;
}
SEXP BG_rowsums5(SEXP x) {
  if(TYPEOF(x) != REALSXP) error("bad types");
  SEXP dim = PROTECT(getAttrib(x, R_DimSymbol));
  if(XLENGTH(dim) != 2) error("bad dims");

  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];

  SEXP res = PROTECT(allocVector(REALSXP, rows));
  long double *rans;
  rans = Calloc(rows, long double);

  double *p = REAL(x);
  int block = 4;
  int col4max = (cols / block) * block;

  for(int i = 0; i < col4max; i += block) {
    long double *s = rans;
    double *q = p + (i * rows);
    for(int j = 0; j < rows; ++j, ++q, ++s) {
      double *r = q;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r;
    }
  }
  for(int i = col4max; i < cols; ++i) {
    long double *s = rans;
    double *q = p + (i * rows);
    for(int j = 0; j < rows; ++j) {
      *s++ += *q++;
    }
  }
  long double *s = rans;
  double *q = REAL(res);
  for(int i = 0; i < rows; ++i) {
    *q++ = (double)*s++;
  }
  Free(rans);
  UNPROTECT(2);
  return res;
}
SEXP BG_rowsums6(SEXP x) {
  if(TYPEOF(x) != REALSXP) error("bad types");
  SEXP dim = PROTECT(getAttrib(x, R_DimSymbol));
  if(XLENGTH(dim) != 2) error("bad dims");

  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];

  SEXP res = PROTECT(allocVector(REALSXP, rows));
  long double *rans;
  rans = Calloc(rows, long double);

  double *p = REAL(x);
  int block = 8;
  int col4max = (cols / block) * block;

  for(int i = 0; i < col4max; i += block) {
    long double *s = rans;
    double *q = p + (i * rows);
    for(int j = 0; j < rows; ++j, ++q, ++s) {
      double *r = q;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r; r += rows;
      *s += *r;
    }
  }
  for(int i = col4max; i < cols; ++i) {
    long double *s = rans;
    double *q = p + (i * rows);
    for(int j = 0; j < rows; ++j) {
      *s++ += *q++;
    }
  }
  long double *s = rans;
  double *q = REAL(res);
  for(int i = 0; i < rows; ++i) {
    *q++ = (double)*s++;
  }
  Free(rans);
  UNPROTECT(2);
  return res;
}
SEXP BG_rowsums7(SEXP x) {
  if(TYPEOF(x) != REALSXP) error("bad types");
  SEXP dim = PROTECT(getAttrib(x, R_DimSymbol));
  if(XLENGTH(dim) != 2) error("bad dims");

  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];

  SEXP res = PROTECT(allocVector(REALSXP, rows));
  long double *rans;
  rans = Calloc(rows, long double);

  double *p = REAL(x);
  int block = 2;
  int col4max = (cols / block) * block;

  for(int i = 0; i < col4max; i += block) {
    long double *s = rans;
    double *q = p + (i * rows);
    for(int j = 0; j < rows; ++j, ++q, ++s) {
      double *r = q;
      *s += *r; r += rows;
      *s += *r;
    }
  }
  for(int i = col4max; i < cols; ++i) {
    long double *s = rans;
    double *q = p + (i * rows);
    for(int j = 0; j < rows; ++j) {
      *s++ += *q++;
    }
  }
  long double *s = rans;
  double *q = REAL(res);
  for(int i = 0; i < rows; ++i) {
    *q++ = (double)*s++;
  }
  Free(rans);
  UNPROTECT(2);
  return res;
}


