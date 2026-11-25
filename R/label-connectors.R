label_anchor_choices <- c(`Original position` = "original", `Previous position` = "previous")

label_anchor_mode <- function(project) {
  if (identical(project$label_anchor_mode, "previous")) "previous" else "original"
}

label_connector <- function(x, y, offset, target, anchor = NULL) {
  if (is.null(anchor)) anchor <- c(0, 0)
  distance <- offset - anchor
  if (all(distance == 0)) return(NULL)
  grid::grob(x = x + grid::unit(anchor[1L], "mm"), y = y + grid::unit(anchor[2L], "mm"),
             offset = distance, target = target, cl = "figure_label_connector")
}

drawDetails.figure_label_connector <- function(x, recording) {
  direction <- -x$offset / sqrt(sum(x$offset^2))
  angle <- atan2(direction[2L], direction[1L]) * 180 / pi %% 360
  start <- c(grid::convertX(x$x, "mm", valueOnly = TRUE),
             grid::convertY(x$y, "mm", valueOnly = TRUE))
  end <- c(grid::convertX(grid::grobX(x$target, angle), "mm", valueOnly = TRUE),
           grid::convertY(grid::grobY(x$target, angle), "mm", valueOnly = TRUE)) + 0.8 * direction
  if (all(is.finite(c(start, end))) && sum((start - end) * direction) > 0) {
    grid::grid.draw(grid::segmentsGrob(grid::unit(start[1L], "mm"), grid::unit(start[2L], "mm"),
      grid::unit(end[1L], "mm"), grid::unit(end[2L], "mm"),
      gp = grid::gpar(col = "#555555", lwd = 0.8, lty = 1, alpha = 1)), recording = FALSE)
  }
}
