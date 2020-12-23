source('content/post/2020-12-15-rtini-ii/script/prototype.c')
library(rtini)

writeLines('generating data')

eltif <- raster::raster("~/Downloads/dem_01.tif")
eldat <- raster::extract(eltif,raster::extent(eltif),buffer=10000)
elmat1 <- matrix(eldat, nrow=ncol(eltif), ncol=nrow(eltif))

# generate large matrix
map <- elmat1
n <- 4
for(i in seq_len(n)) {
  map <- cbind(map, map[, rev(seq_len(ncol(map)))])
  map <- rbind(map, map[rev(seq_len(nrow(map))),])
}
# Need a few more cols to get to 8192 x 8193
map <- cbind(map[, rev(seq_len(200))], map)

# When comparing rtini to martini that the output meshes may not be in the
# exact same order even within each triangle, so we need to normalize them.
# This may mess up the winding, but that doesn't matter for this test.

order_tris <- function(x) {
  x <- unlist(x)
  x <- x[x > 0]
  tri.n <- length(x) / 3
  x <- x[order(x, rep(seq_len(tri.n), each=3), x)]
  x <- matrix(x, nrow=3)
  x[, order(x[1,], x[2,], x[3,])]
}
sys.time <- function(exp, reps=11) {
  res <- matrix(0, reps, 5)
  time.call <- quote(system.time({NULL}))
  time.call[[2]][[2]] <- substitute(exp)
  gc()
  for(i in seq_len(reps)) {
    res[i,] <- eval(time.call, parent.frame())
  }
  structure(res, class='proc_time2')
}
# Now we can subset to desired target sizes
ks <- 2 ^ (6:13) + 1
# ks <- 2 ^ 6 + 1
tol <- 10
library(V8)
ct <- v8()
ct$source('content/post/2020-12-15-rtini-ii/script/prototype.js')

res <- vector('list', length(ks))
## Try different sizes
# for(ki in seq_along(ks)) {
#   k <- ks[ki]
#   m <- map[seq_len(k), seq_len(k)]
#
#   writeLines(sprintf("doing R %d %s", k, Sys.time()))
#   t.r <- sys.time(err <- rtini_error(m))
#   writeLines(sprintf("doing R extract %d %s", k, Sys.time()))
#   t.r.e <- sys.time(approx <- rtini_extract(err, tol=tol))
#
#   writeLines(sprintf("doing JS %d %s",k, Sys.time()))
#   ct$assign('map', c(m))
#   cmd1 <- sprintf("var err = comp_errors(map, %d);", k)
#   cmd2 <- sprintf("var approx = updatedGeometry(err, %d, %f);", k, tol)
#
#   t.js <- sys.time(ct$eval(cmd1))
#   writeLines(sprintf("doing JS extract %d %s",k, Sys.time()))
#   t.js.e <- sys.time((ct$eval(cmd2)))
#   err.js <- ct$get('err')
#   approx.js <- ct$get('approx')
#
#   r.vs.js <- all.equal(order_tris(approx), order_tris(approx.js))
#   if(!isTRUE(r.vs.js)) warning("R vs JS unequal: ", r.vs.js)
#
#   writeLines(sprintf("doing C %d %s", ks[ki], Sys.time()))
#   # we have only implemented the error calc in C
#   t.c <- sys.time(err.c <- compute_errorc(m, k))
#   r.vs.c <- all.equal(err, err.c)
#   if(!isTRUE(r.vs.js)) warning("R vs JS unequal: ", r.vs.js)
#
#   res[[ki]] <- list(
#     r=t.r[,3], js=t.js[,3], c=t.c[,3], r.e=t.r.e[,3], js.e=t.js.e[,3]
#   )
# }

## Try different tolerances
k <- 1025
m <- map[seq_len(k), seq_len(k)]
tols <- c(.5, 1, 2, 5, 10, 20)

res <- lapply(
  tols,
  function(tol) {
    writeLines(sprintf("doing R %d %s", k, Sys.time()))
    t.r <- sys.time(err <- rtini_error(m))
    writeLines(sprintf("doing R extract %d %s", k, Sys.time()))
    t.r.e <- sys.time(approx <- rtini_extract(err, tol=tol))

    writeLines(sprintf("doing JS %d %s",k, Sys.time()))
    ct$assign('map', c(m))
    cmd1 <- sprintf("var err = comp_errors(map, %d);", k)
    cmd2 <- sprintf("var approx = updatedGeometry(err, %d, %f);", k, tol)

    t.js <- sys.time(ct$eval(cmd1))
    writeLines(sprintf("doing JS extract %d %s",k, Sys.time()))
    t.js.e <- sys.time((ct$eval(cmd2)))
    err.js <- ct$get('err')
    approx.js <- ct$get('approx')

    list(tol=tol, k=k, r=t.r, r.e=t.r.e, js=t.js, js.e=t.js.e)
  }
)
err <- rtini_error(m)
polys <- lapply(tols, rtini_extract, error=err)
polylen <- lengths(lapply(polys, unlist)) / 3

r.e <- lapply(res, function(x) x$r.e[,3])
js.e <- lapply(res, function(x) x$js.e[,3])

poly.time <- do.call(
  rbind,
  c(
    Map(data.frame, time=r.e, polys=polylen, Language='R'),
    Map(data.frame, time=js.e, polys=polylen, Language='JS')
  )
)
r <- lapply(res, function(x) x$r[,3])
js <- lapply(res, function(x) x$js[,3])

err.time <- do.call(
  rbind,
  c(
    Map(data.frame, time=r, Language='R', Grid='1025x1025'),
    Map(data.frame, time=js, Language='JS', Grid='1025x1025')
  )
)
# saveRDS(poly.time, 'content/post/2020-12-15-rtini-ii/data/bench-poly-2.RDS')
# saveRDS(err.time, 'content/post/2020-12-15-rtini-ii/data/bench-poly.RDS')
# err.time <- readRDS('data/bench-poly.RDS')
# poly.time <- readRDS('data/bench-poly-2.RDS')
title <- theme(title=element_text(size=9))
coord <- coord_cartesian(ylim=c(0, .6))
p1 <- ggplot(poly.time) + 
  geom_point(aes(x=polys/1e3, y=time, color=Language), alpha=.5) +
  ggtitle('Mesh Extraction') + coord + title +
  scale_color_manual(values=c(JS='#fc8d62',R='#8da0cb')) +
  xlab("Polygons (K)") + ylab("Seconds")
p2 <- ggplot(err.time) + 
  geom_point(aes(x=Grid, y=time, color=Language), alpha=.5) +
  ggtitle('Error Calc') +
  ylab(FALSE) + coord + title +
  scale_color_manual(values=c(JS='#fc8d62',R='#8da0cb')) +
  theme(
    axis.title.y=element_blank(), axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
library(patchwork)
p1 + p2 + plot_layout(guides='collect') +
  plot_annotation(title='RTIN Mesh Extraction vs. Poly Count') &
  theme(legend.position='bottom') &
  plot_layout(widths=c(3,1))

stop()

f <- tempfile()
xx <- build_der_mesh(m, err, tol=10, f)

library(rayrender)
render_scene(
  fov=18,
  width=800, height=800,
  samples=10,
  group_objects(
    obj_model(f, vertex_colors=TRUE),
    # obj_model(meshes[length(meshes)], vertex_colors=TRUE),
    pivot_point=numeric(3), group_angle=c(90, 0, 180),
    group_order_rotation=c(3, 1, 2),
  ),
  lookfrom=c(0,3,2)
)



