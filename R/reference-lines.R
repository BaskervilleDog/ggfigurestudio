reference_move_key <- function(layer, index) {
  id <- layer$figure_reference_id
  paste0("element/reference/", if (is.null(id)) index else id)
}

shift_reference_line <- function(grob, offset) {
  if (is.null(offset)) return(grob)
  dx <- grid::unit(offset[1L], "mm")
  dy <- grid::unit(offset[2L], "mm")
  if (inherits(grob, "segments")) {
    grob$x0 <- grob$x0 + dx
    grob$x1 <- grob$x1 + dx
    grob$y0 <- grob$y0 + dy
    grob$y1 <- grob$y1 + dy
  } else if (inherits(grob, "polyline")) {
    grob$x <- grob$x + dx
    grob$y <- grob$y + dy
  }
  grob
}

reference_hit <- function(grob, key, bounds, move_key, axis) {
  path <- if (inherits(grob, "segments")) {
    segment_hit(grob, key, bounds, radius = 1.4)
  } else {
    pos <- grid::deviceLoc(grob$x, grob$y, valueOnly = TRUE)
    list(key = key, x = pos$x, y = pos$y, bounds = bounds, line = TRUE, radius = 1.4)
  }
  path$move_key <- move_key
  path$move_axis <- axis
  path
}
