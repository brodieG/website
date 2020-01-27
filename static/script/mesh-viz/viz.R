library(vetr)

source('static/script/mesh-viz/rtin2.R')
source('static/script/mesh-viz/extract-vec.R')
source('static/script/mesh-viz/rtin-vec.R')
source('static/script/mesh-viz/viz-lib.R')

vsq <- matrix(0, 65, 65)
vsq[1:65, 3:63] <- volcano[1:65,1:61]
vsq[1:65, 1:2] <- volcano[1:65, 1]
vsq[1:65, 64:65] <- volcano[1:65, 61]

errors <- compute_error(vsq)
elmax <- diff(range(vsq))

# ------------------------------------------------------------------------------
# - Recorded  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# library(rgl)
# par3d(windowRect=c(20,20,400,400))
# mesh.obj.3 <- tris_to_obj(tris3, map)
# writeLines(mesh.obj.3, f3)
# plot3d(readOBJ(f1), col='grey50')

