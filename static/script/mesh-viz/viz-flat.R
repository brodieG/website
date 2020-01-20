source('static/script/mesh-viz/viz-batch-init.R')

err.frac <- elmax/c(30,15,3)
# mesh.colors <- c('gold', 'grey65', '#DD4F12')
gold <- '#CCAC00'
metal.col <-  c(gold, 'grey50', '#CC3322')
mesh.colors <- metal.col

errors <- rtini::rtini_error(vsq)
meshes <- lapply(err.frac, extract_mesh2, errors=errors)
lwd <- c(5, 4, 3)
de <- dim(errors)

plot_tri_ids(
  meshes[[1]], de, new=TRUE, lwd=4, mai=rep(.1, 4), xpd=NA, col=metal.col[[1]]
)
plot_tri_ids(meshes[[2]], de, lwd=2, col=metal.col[[2]], new=FALSE)
plot_tri_ids(meshes[[3]], de, lwd=1, col=metal.col[[3]], new=FALSE)


par(mfrow=c(1,3))
plot_tri_ids(
  meshes[[1]], de, lwd=1, col=metal.col[[1]], new=TRUE, mai=rep(.1, 4), xpd=NA
)
plot_tri_ids(
  meshes[[2]], de, lwd=1, col=metal.col[[2]], new=TRUE, mai=rep(.1, 4), xpd=NA
)
plot_tri_ids(
  meshes[[3]], de, lwd=1, col=metal.col[[3]], new=TRUE, mai=rep(.1, 4), xpd=NA
)
