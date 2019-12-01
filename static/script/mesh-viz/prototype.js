
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
  paste0('terrain = [', paste0(map, collapse=','), ']'),
  '~/Downloads/map-js.json'
)
```

JSON.stringify(Array.from(errors));

// right click, copy object, then

```{r}
errors3 <- array(unlist(jsonlite::fromJSON(json)), dim(map))
```

*/

{
  a = performance.now();
  errors = comp_errors(terrain, 257, 256);
  performance.now() - a;
}
{
  a = performance.now();
  coords = updatedGeometry(errors, 257, 256, tol);
  performance.now() - a;
}

/* Error: Accessing TypedArray data over Xrays is slow, and forbidden in order to encourage performant code. To copy TypedArrays across origin boundaries, consider using Components.utils.cloneInto(). */
