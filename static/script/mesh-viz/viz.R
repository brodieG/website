library(vetr)

source('static/script/mesh-viz/rtin2.R')
source('static/script/mesh-viz/rtin-vec.R')
source('static/script/mesh-viz/viz-lib.R')

# - Base Colors ----------------------------------------------------------------

gold <- '#CCAC00'
metal.col <-  c(gold, 'grey75', '#CC3322')
mesh.colors <- metal.col

stop('loaded')

# ------------------------------------------------------------------------------
# - Recorded  -------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(rgl)
par3d(windowRect=c(20,20,400,400))
mesh.obj.3 <- tris_to_obj(tris3, map)
writeLines(mesh.obj.3, f3)
plot3d(readOBJ(f1), col='grey50')

