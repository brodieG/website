/*
 * Adapted from Vladimir Agafonkin's post:
 *
 * https://observablehq.com/@mourner/martin-real-time-rtin-terrain-mesh
 *
 * To simpify interchange changed format to be a normal array instead of
 * Float32.  Also makes it a bit more apples to apples.
 */

function comp_errors(terrain, gridSize) {
  const errors = new Array(gridSize * gridSize).fill(0);
  const tileSize = gridSize - 1;
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
/*
 * We initialize a maximal size array (might be too big).
 *
 * Return value is 1 indexed for use in R, but also to make it easy to
 * distinguish the unused part of the array
 */
function updatedGeometry(errors, gridSize, maxError) {
  let i = 0;
  const tileSize = gridSize - 1;
  const indices = new Array(tileSize * tileSize * 2 * 3).fill(0);

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
      indices[i++] = ay * gridSize + ax + 1;
      indices[i++] = by * gridSize + bx + 1;
      indices[i++] = cy * gridSize + cx + 1;
    }
  }
  processTriangle(0, 0, tileSize, tileSize, tileSize, 0);
  processTriangle(tileSize, tileSize, 0, 0, 0, tileSize);

  return indices;
}
