# Generate the Rlang "Bag"  Want two sets of hex coordinates, the front, back,
# and want to generate triangles for the back and sides.  Need to compute from
# our viewpoint and project out the back hexagon.
#
# Challenge, compute what elements in our full scene are actually inside.
#
# This was the original approach when we thought we wanted the empty ring, but
# we abandoned that because unclear that visually it would be compelling.
#
# @param depth how far from the hex edge to the corresponding vertex in the back
#   hexagon.
# @param y numeric(1L) y coord of original hexagon parallel to x-z plane
# @param obs the observer coordinates (x,y,z)
# @param hex the coordinates of the hex, hex assumed closed, first coordinate is
#   taken to be X, second Z, and the Y value is assumed to be 0.

comp_inside <- function(
  hex, hex.back, light_index=1, material=diffuse(color='red')
) {
  vetr::vetr(
    structure(list(numeric(7), numeric(7)), class='data.frame'),
    structure(list(numeric(7), numeric(7), numeric(7)), class='data.frame') &&
      nrow(.) == nrow(hex),
    numeric(1)
  )
  hex.in <- rbind(hex[[1]], 0, hex[[2]])
  hex.out <- t(as.matrix(hex.back[c(1,3,2)]))

  i <- seq_len(ncol(hex.in) - 1L)
  ii <- seq_len(ncol(hex.in) - 1L) + 1L   # i + 1
  area_s <- sum(hex.in[1,i] * hex.in[3,ii] - hex.in[1,ii] * hex.in[3,i]) / 2

  ccw <- area_s >= 0  # treat degenerates as counter-clockwise
  flip <- !ccw

  # code adapted from rayrender

  sides <- unlist(
    lapply(
      seq_len(ncol(hex.in) - 1L),
      function(i) {
        if(i == light_index) material=light(intensity=5)
        list(
          triangle(
            v1=hex.in[,i], v2=hex.out[,i], v3=hex.out[,i+1],
            material = material, reversed = flip
          ),
          triangle(
            v1=hex.in[,i], v2=hex.out[,i+1], v3=hex.in[,i+1],
            material = material, reversed = flip
          )
    ) } ),
    recursive=FALSE
  )
  back.idx <- matrix(decido::earcut(hex), 3)
  back.tris <- lapply(
    split(back.idx, col(back.idx)),
    function(i)
      triangle(
        hex.out[,i[1]], hex.out[,i[2]], hex.out[,i[3]],
        material=material
        # material=diffuse('#553333')
      )
  )
  dplyr::bind_rows(c(sides, back.tris))
}

