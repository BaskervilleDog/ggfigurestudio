mark_overlay <- function(paths, width, height) {
  # SVG uses millimetres as user units so hit widths are independent of plot size.
  paths <- paths[order(vapply(paths, function(path) isTRUE(path$priority), logical(1)))]
  clips <- list()
  panel_clip <- function(bounds) {
    bounds <- bounds * 25.4
    clip_key <- paste(bounds, collapse = "/")
    clip <- clips[[clip_key]]
    if (is.null(clip)) {
      clip <- shiny::tags$clipPath(id = paste0("figure-area-clip-", length(clips) + 1L),
        shiny::tags$rect(x = bounds[1], y = height - bounds[4],
          width = bounds[2] - bounds[1], height = bounds[4] - bounds[3]))
      clips[[clip_key]] <<- clip
    }
    paste0("url(#", clip$attribs$id, ")")
  }
  shapes <- lapply(paths, function(path) {
    x <- path$x * 25.4
    y <- height - path$y * 25.4
    if (!is.null(path$polygons)) {
      if (!length(path$polygons)) return(NULL)
      d <- paste(vapply(path$polygons, function(rows) {
        points <- paste(sprintf("%.4f %.4f", x[rows], y[rows]), collapse = " ")
        paste("M", points, "Z")
      }, character(1)), collapse = " ")
      return(shiny::tags$path(d = d, fill = "transparent",
        `fill-rule` = if (isTRUE(path$evenodd)) "evenodd",
        `clip-path` = panel_clip(path$bounds),
        `data-inspect` = path$key, `data-outline` = d, `data-move-key` = path$move_key,
        class = "figure-mark-hit",
        style = if (length(path$move_key)) "pointer-events:fill;cursor:move;touch-action:none"
          else "pointer-events:fill;cursor:pointer"))
    }
    keep <- is.finite(x) & is.finite(y)
    if (length(path$bounds) && is.null(path$move_axis)) {
      bounds <- path$bounds * 25.4
      keep <- keep & x >= bounds[1] & x <= bounds[2] &
        y >= height - bounds[4] & y <= height - bounds[3]
    }
    if (!any(keep) || (path$line && sum(keep) < 2L)) return(NULL)
    if (path$line) {
      move <- !c(FALSE, head(keep, -1L))
      d <- paste(sprintf("%s %.4f %.4f", ifelse(move[keep], "M", "L"), x[keep], y[keep]), collapse = " ")
      outline <- d
      radius <- path$radius
    } else {
      d <- paste(sprintf("M %.4f %.4f h 0.001", x[keep], y[keep]), collapse = " ")
      radius <- max(path$radius[is.finite(path$radius)], 1.3)
      outline <- paste(sprintf(paste0(
        "M %.4f %.4f a %.4f %.4f 0 1 0 %.4f 0 a %.4f %.4f 0 1 0 %.4f 0"),
        x[keep] - radius, y[keep], radius, radius, 2 * radius,
        radius, radius, -2 * radius), collapse = " ")
    }
    shiny::tags$path(d = d, fill = "none", stroke = "transparent",
                     `stroke-width` = 2 * radius, `stroke-linecap` = "round",
                     `data-move-key` = path$move_key, `data-move-axis` = path$move_axis,
                     `clip-path` = if (length(path$move_axis)) panel_clip(path$bounds),
                     `data-inspect` = path$key, `data-outline` = outline, class = "figure-mark-hit",
                     style = "pointer-events:stroke;cursor:pointer")
  })
  shiny::tags$svg(viewBox = sprintf("0 0 %f %f", width, height), `aria-hidden` = "true",
    style = "position:absolute;inset:0;width:100%;height:100%;pointer-events:none;z-index:1",
    if (length(clips)) shiny::tags$defs(unname(clips)),
    shapes, shiny::tags$g(class = "figure-mark-selection", style = "pointer-events:none"))
}
