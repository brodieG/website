source('static/script/mesh-viz/viz-lib.R')

# Build Mesh From Elevation Data, Errors, and Tolerance
#
# Works by side effect of writing to tempfile.
#
# @param map the elevation map
# @param err matrix of errors computed with rtini_error
# @param tol a tolerance to extract mesh at
# @param file name of file to write to
# @return a list with the file name and the xyz data

build_der_mesh <- function(map, err, tol, file=tempfile()) {
  ids <- rtini_extract(err, tol=tol)
  tris <- rbind(do.call(cbind, ids), NA)

  xyz <- ids_to_xyz(ids, map, scale=NULL)
  xyz2 <- xyz
  max.xy <- max(unlist(xyz[c('x', 'y')]))
  xyz$x <- (xyz$x - mean(range(xyz$x))) / max.xy
  xyz$y <- (xyz$y - mean(range(xyz$y))) / max.xy
  xyz$z <- xyz$z / max(map) / 3

  # Make sure these are all counterclockwise by checking triangle area sign.
  # Assumes nothing steeper than vertical.

  i <- 1:3
  ii <- c(2:3,1)
  base <- matrix(rep(seq(0, length(xyz[[1]]) - 1, by=3), each=3), 3)
  iv <- i + base
  iiv <- ii + base
  area_s <- with(xyz, colSums(matrix((x[iv] * y[iiv] - x[iiv] * y[iv]) / 2, 3)))
  ccw <- area_s >= 0  # treat degenerates as counter-clockwise
  if(!all(ccw)) stop('bad triangle winding')

  # Add color depending on Z value
  pos.col <- c(col2rgb('lightgreen') / 255)
  neg.col <- c(col2rgb('grey95') / 255)
  mesh0 <- xyz_to_mesh(xyz)
  mesh1 <- mesh_skirt(mesh0, vcolor=NULL)
  mesh.col <- matrix(
    unlist(
      lapply(
        mesh1[, 'z'],
        function(x) {
          res <- matrix(numeric(), 3, length(x))
          res[, x > 0] <- pos.col
          res[, x < 0] <- neg.col
          asplit(res, 1)
        }
      ),
      recursive=FALSE
    ),
    nrow=3,
    byrow=TRUE
  )
  mesh <- cbind(mesh1, mesh.col)

  obj <- mesh_to_obj(mesh)
  writeLines(obj, file)
  list(file=file, xyz=xyz)
}


