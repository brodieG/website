/*
f <- inline::cfunction(sig=c(terr='numeric', grid='integer'), body="

")
*/
compute_errorc <- inline::cfunction(sig=c(terr='numeric', grid='integer'), body="
//SEXP function comp_errors(SEXP terr, SEXP grid) {
  int gridSize = asInteger(grid);
  int tileSize = gridSize - 1;
  SEXP errSxp = PROTECT(allocVector(REALSXP, gridSize * gridSize));
  double * terrain = REAL(terr);
  double * errors = REAL(errSxp);
  errors[0] = errors[gridSize - 1] = errors[gridSize * gridSize - 1] =
    errors[gridSize * gridSize - gridSize] = 0;

  int numSmallestTriangles = tileSize * tileSize;
  // 2 + 4 + 8 + ... 2^k = 2 * 2^k - 2
  int numTriangles = numSmallestTriangles * 2 - 2; 
  int lastLevelIndex = numTriangles - numSmallestTriangles;

  // iterate over all possible triangles, starting from the smallest level
  for (int i = numTriangles - 1; i >= 0; i--) {

    // get triangle coordinates from its index in an implicit binary tree
    int id = i + 2;
    int ax = 0, ay = 0, bx = 0, by = 0, cx = 0, cy = 0;
    if (id & 1) {
      bx = by = cx = tileSize; // bottom-left triangle
    } else {
      ax = ay = cy = tileSize; // top-right triangle
    }
    while ((id >>= 1) > 1) {
      int mx = (ax + bx) >> 1;
      int my = (ay + by) >> 1;

      if (id & 1) { // left half
        bx = ax; by = ay;
        ax = cx; ay = cy;
      } else { // right half
        ax = bx; ay = by;
        bx = cx; by = cy;
      }
      cx = mx; cy = my;
    }
    // calculate error in the middle of the long edge of the triangle
    double interpolatedHeight =
      (terrain[ay * gridSize + ax] + terrain[by * gridSize + bx]) / 2;
    int middleIndex = ((ay + by) >> 1) * gridSize + ((ax + bx) >> 1);
    double middleError = abs(interpolatedHeight - terrain[middleIndex]);

    if (i >= lastLevelIndex) { // smallest triangles
      errors[middleIndex] = middleError;

    } else { // bigger triangles; accumulate error with children
      double leftChildError = 
        errors[((ay + cy) >> 1) * gridSize + ((ax + cx) >> 1)];
      double rightChildError = 
        errors[((by + cy) >> 1) * gridSize + ((bx + cx) >> 1)];

      double tmp = errors[middleIndex];
      tmp = tmp > middleError ? tmp : middleError;
      tmp = tmp > leftChildError ? tmp : leftChildError;
      tmp = tmp > rightChildError ? tmp : rightChildError;
      errors[middleIndex] = tmp;
    }
  }
  SEXP dim = PROTECT(allocVector(INTSXP, 2));
  INTEGER(dim)[0] = gridSize;
  INTEGER(dim)[1] = gridSize;
  setAttrib(errSxp, R_DimSymbol, dim);
  UNPROTECT(2);
  return errSxp;
//}
")

