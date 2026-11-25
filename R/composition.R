figure_composition <- function(plots, width = 240, height = 150, ncol = min(2L, length(plots)), gap = 4) {
  if (!is.list(plots) || !length(plots) ||
      !all(vapply(plots, inherits, logical(1), c("ggplot", "figure_project"))))
    stop("Supply a list of ggplot objects or figure projects.")
  labels <- names(plots)
  if (is.null(labels)) labels <- rep("", length(plots))
  unnamed <- is.na(labels) | !nzchar(labels)
  labels[unnamed] <- paste("Plot", which(unnamed))
  children <- lapply(plots, function(plot) {
    if (inherits(plot, "figure_project")) plot else figure_project(plot)
  })
  names(children) <- make.unique(labels)
  project <- structure(list(plots = children, width = width, height = height,
                            ncol = ncol, gap = gap), class = "figure_composition")
  composition_cells(project)
  project
}

composition_cells <- function(project) {
  positive <- function(value) is.numeric(value) && length(value) == 1L &&
    is.finite(value) && value > 0
  if (!positive(project$width) || !positive(project$height))
    stop("Image width and height must be positive.")
  cols <- project$ncol
  gap <- project$gap
  if (!positive(cols) || cols != trunc(cols) || cols > length(project$plots))
    stop("Choose a whole number of columns between 1 and the number of plots.")
  if (!is.numeric(gap) || length(gap) != 1L || !is.finite(gap) || gap < 0)
    stop("Enter a nonnegative gap in millimetres.")
  rows <- ceiling(length(project$plots) / cols)
  width <- (project$width - (cols - 1) * gap) / cols
  height <- (project$height - (rows - 1) * gap) / rows
  if (width <= 0 || height <= 0) stop("The gap leaves no room for the plots.")
  list(width = width, height = height, columns = cols)
}

render_composition <- function(project) {
  cells <- composition_cells(project)
  children <- lapply(seq_along(project$plots), function(i) {
    col <- (i - 1L) %% cells$columns
    row <- (i - 1L) %/% cells$columns
    x <- col * (cells$width + project$gap)
    y <- project$height - row * (cells$height + project$gap)
    grid::grobTree(render_figure(project$plots[[i]]),
      vp = grid::viewport(x = x / project$width, y = y / project$height,
        width = cells$width / project$width, height = cells$height / project$height,
        just = c("left", "top"), clip = "on"))
  })
  grid::gTree(children = do.call(grid::gList, children))
}

composition_controls <- function(project) {
  editor_group("Multi-plot image",
    editor_select("composition_plot", "Plot to edit",
      stats::setNames(as.character(seq_along(project$plots)), names(project$plots)), selected = "1"),
    editor_actions(shiny::actionButton("edit_composition_plot", "Edit selected plot")),
    editor_fields(shiny::numericInput("composition_columns", "Columns", project$ncol,
                  min = 1, max = length(project$plots), step = 1),
                  shiny::numericInput("composition_gap", "Gap (mm)", project$gap, min = 0)),
    editor_actions(shiny::actionButton("apply_composition_layout", "Apply layout")),
    shiny::checkboxInput("show_composition", "Preview combined image", FALSE),
    editor_status("composition_status"),
    description = "Edit one plot at a time. The combined image uses the order in the list you supplied in R.")
}

composition_preview <- function(project) {
  canvas <- svglite::svgstring(project$width / 25.4, project$height / 25.4)
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device))
  grid::grid.draw(render_composition(project))
  grDevices::dev.off(device)
  shiny::tags$img(alt = "Combined image preview",
    style = sprintf("width:%fmm;max-width:100%%;height:auto", project$width),
    src = paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(charToRaw(enc2utf8(as.character(canvas()))))))
}
