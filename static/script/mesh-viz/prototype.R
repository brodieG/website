# File originally from http://tylermw.com/data/dem_01.tif.zip
eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

# map <- elmat1[1:5, 1:5]
# map <- elmat1[1:17, 1:17]
# map <- elmat1[1:257, 1:257]
# map <- elmat1[1:5, 1:9]
# map <- elmat1[1:(2*4+1), 1:(2*3+1)]  # smallest error?
# map <- elmat1[1:(2*5+1), 1:(2*4+1)]
map <- elmat1[1:(2*3+1), 1:(2*4+1)]
# map <- elmat1[1:11, 1:15]
map <- elmat1[1:13, 1:11]
# map <- volcano
# map <- elmat1[-1,]
map <- elmat1[1:257,1:257]
system.time(errors <- compute_error(map))
# tol <- diff(range(map)) / 50
tol <- diff(range(map))
# debug(extract_mesh2)
system.time(tris <- extract_mesh2(errors, tol))
# treeprof::treeprof((tris <- extract_mesh2(errors, tol))
plot_tri_ids(tris, dim(errors))
plot_points_ids(which(errors > tol), dim(map))


# # debugging code
# writeLines(
#   paste0(
#     'terrain = [', paste0(map, collapse=','),
#     sprintf('];
#     JSON.stringify(comp_errors(terrain, %d, %d));
#     ', nrow(map), nrow(map) - 1
# ) ) )
# errors3 <- array(unlist(jsonlite::fromJSON(json)), dim(map))

# For each error, we need to draw all the corresponding triangles.  This means
# we need to figure out the size of the "diagonal", and whether we're dealing
# with a diamond or square.


# we need to compute even level odd level tiles.  For odd tiles (1 index), we
# check the actual diagonal, i.e. tile is square.  For even tiles, we treat the
# tile as a rhombus so the diagonal is actually parallel to the x axis (or y
# axis depending on how you draw it)?

# Version for animation
#
# We'll want to track:
#
# * ax, ay, bx, by,
# * cx, cy, mx, my
# * az, bz, mz
# * *Error?
# * lcx, lcy, rcx, rcy
# * errors array
#
# We'll display the errors array, and the terrain array overlaid with the
# triangle tracking.

map <- elmat1[1:5, 1:5]
map <- elmat1[1:17, 1:17]
map <- elmat1[1:257, 1:257]
system.time(errors2 <- errors_rtin(map))
errors <- compute_error(map)
all.equal(errors, errors2)

set.seed(1221)
m2 <- map
m3 <- matrix(round(runif(513*513) * 100, 0), 513)
m4 <- matrix(round(runif(1025*1025) * 100, 0), 1025)
m5 <- matrix(round(runif(2049*2049) * 100, 0), 2049)
m6 <- matrix(round(runif(4097*4097) * 100, 0), 4097)
m3 <- matrix(round(runif(8193*8193) * 100, 0), 8193)



mm <- m7
gc()
system.time(compute_error(m7))
gc()
system.time(compute_error3(m7))
gc()
system.time(compute_errorc(m7, nrow(m7)))

bench::mark(
  compute_error3(m5), compute_error(m4), 
)
treeprof::treeprof(compute_error3(m6))


## Messing With Watching Algo

map <- elmat1[1:3, 1:3]
