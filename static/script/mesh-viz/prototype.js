
function comp_errors(terrain, gridSize, tileSize) {
  const errors = new Float32Array(gridSize * gridSize);

  const numSmallestTriangles = tileSize * tileSize;
  const numTriangles = numSmallestTriangles * 2 - 2; // 2 + 4 + 8 + ... 2^k = 2 * 2^k - 2
  const lastLevelIndex = numTriangles - numSmallestTriangles;

  // iterate over all possible triangles, starting from the smallest level
  for (let i = numTriangles - 1; i >= 0; i--) {

    // get triangle coordinates from its index in an implicit binary tree
    let id = i + 2;
    let ax = 0, ay = 0, bx = 0, by = 0, cx = 0, cy = 0;
    if (id & 1) {
      bx = by = cx = tileSize; // bottom-left triangle
    } else {
      ax = ay = cy = tileSize; // top-right triangle
    }
    while ((id >>= 1) > 1) {
      const mx = (ax + bx) >> 1;
      const my = (ay + by) >> 1;

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
    const interpolatedHeight = (terrain[ay * gridSize + ax] + terrain[by * gridSize + bx]) / 2;
    const middleIndex = ((ay + by) >> 1) * gridSize + ((ax + bx) >> 1);
    const middleError = Math.abs(interpolatedHeight - terrain[middleIndex]);

    if (i >= lastLevelIndex) { // smallest triangles
      errors[middleIndex] = middleError;

    } else { // bigger triangles; accumulate error with children
      const leftChildError = errors[((ay + cy) >> 1) * gridSize + ((ax + cx) >> 1)];
      const rightChildError = errors[((by + cy) >> 1) * gridSize + ((bx + cx) >> 1)];
      errors[middleIndex] = Math.max(errors[middleIndex], middleError, leftChildError, rightChildError);
    }
  }
  return errors;
}
function updatedGeometry(errors, gridSize, tileSize, maxError) {
  let i = 0;
  const indices = new Float32Array(gridSize * gridSize);

  function processTriangle(ax, ay, bx, by, cx, cy) {
    // middle of the long edge
    const mx = (ax + bx) >> 1;
    const my = (ay + by) >> 1;

    if (
      Math.abs(ax - cx) + Math.abs(ay - cy) > 1 &&
        errors[my * gridSize + mx] > maxError
    ) {
      // triangle doesn't approximate the surface well enough; split it into two
      processTriangle(cx, cy, ax, ay, mx, my);
      processTriangle(bx, by, cx, cy, mx, my);

    } else {
      // add a triangle to the final mesh
      indices[i++] = ay * gridSize + ax;
      indices[i++] = by * gridSize + bx;
      indices[i++] = cy * gridSize + cx;
    }
  }
  processTriangle(0, 0, tileSize, tileSize, tileSize, 0);
  processTriangle(tileSize, tileSize, 0, 0, 0, tileSize);

  return indices;
}
/*
```{r}
writeLines(
  paste0('var ter_257 = [', paste0(m2, collapse=','), '];'),
  '~/Downloads/map-js-257.json'
)
writeLines(
  paste0('var ter_513 = [', paste0(m3, collapse=','), '];'),
  '~/Downloads/map-js-513.json'
)
writeLines(
  paste0('var ter_1025 = [', paste0(m4, collapse=','), '];'),
  '~/Downloads/map-js-1025.json'
)
writeLines(
  paste0('var ter_2049 = [', paste0(m5, collapse=','), '];'),
  '~/Downloads/map-js-2049.json'
)
writeLines(
  paste0('var ter_4097 = [', paste0(m5, collapse=','), '];'),
  '~/Downloads/map-js-4097.json'
)
writeLines(
  paste0('var ter_8193 = [', paste0(m5, collapse=','), '];'),
  '~/Downloads/map-js-8193.json'
)
```

JSON.stringify(Array.from(errors));

// right click, copy object, then

```{r}
# we compared both 1025 and original map 257 and got exact same results
json <- '~/Downloads/error-js-1025.json'
errors3 <- array(unlist(jsonlite::fromJSON(json)), dim(map))
```

*/

{
  a = performance.now();
  errors = comp_errors(ter_1025, 1025, 1024);
  performance.now() - a;
}
/*
ter_257:
Chrome: 35.7, 25.96, 23.50, 24.134, 20.66, 20.195, 20.62,
20.29, 19.46, 19.32, 19.72, 26.69, 20.14
FF: 50, 49, 42, 40, 42, 42, 42

ter_513:
Chrome:
104.1, 101.1, 101, 90.2, 92.3, 95.4, 92.82, 94,32, 91.99, 95.4, 94.5

ter_1025:
Chrome:
495.32, 499.19, 485.19, 478.67, 475.19, 479.67, 484.8

ter_2049:
2235.6, 2223.76, 2193.44, 2185.69, 2188.02, 2179.47, 2195.48, 2260.02

ter_4096:
7488.19, 7483.48, 7280.48, 7322.34, 7485.29, 7449.80

ter_8193:
29757.15, 29386.23, 29331.4, 28982.32 
*/


{
  tol = 5
  a = performance.now();
  coords = updatedGeometry(errors, 257, 256, tol);
  performance.now() - a;
}

/* Error: Accessing TypedArray data over Xrays is slow, and forbidden in order to encourage performant code. To copy TypedArrays across origin boundaries, consider using Components.utils.cloneInto(). */
