figure_project <- function(plot, width = 180, height = 110) {
  stopifnot(inherits(plot, "ggplot"))
  for (size in list(width, height)) {
    stopifnot(is.numeric(size), length(size) == 1L, is.finite(size), size > 0)
  }
  structure(list(version = 1L, plot = plot, width = width, height = height,
                 title = NULL, dx = 0, dy = 0, palette = "Original",
                 aesthetic = "colour"), class = "figure_project")
}

render_figure <- function(project) {
  if (inherits(project, "figure_composition")) return(render_composition(project))
  build_figure(project)
}

plot_size <- function(project) {
  if (is.null(project$plot_size)) c(width = 100, height = 100) else project$plot_size
}

build_figure <- function(project, capture = NULL) {
  stopifnot(inherits(project, "figure_project"))
  p <- restore_saved_plot(project$plot)
  p <- apply_grid_visibility(p, project$grid_visibility)
  if (!is.null(project$title)) p <- p + ggplot2::labs(title = project$title)
  p <- apply_plot_palettes(p, project_palettes(project))
  built <- ggplot2::ggplot_build(scope_annotation_facets(with_group_style_guides(p)))
  if (!is.null(capture)) capture$annotation_facets <- annotation_facet_choices(built$layout)
  built <- apply_facet_annotation_edits(built)
  previous <- if (label_anchor_mode(project) == "previous") project$previous_label_offsets
  annotation_edits <- grepl("^element/(shape|reference)/", names(project$element_edits))
  facet_styles <- any(vapply(built$data, function(data) length(data$.figure_facet_style) > 0L, logical(1)))
  if (!is.null(capture) || any(annotation_edits) || facet_styles)
    built <- capture_mark_layers(built, capture, project$element_edits[annotation_edits],
                                 connect_labels = isTRUE(project$connect_labels), previous = previous)
  layout <- ggplot2::ggplot_gtable(built)
  if (length(project$tick_edits) || !is.null(capture)) {
    layout <- apply_ticks(layout, built, project$tick_edits, capture)
  }
  if (project$dx != 0 || project$dy != 0) layout <- apply_saved_title_offset(layout, project)
  element_edits <- project$element_edits[!annotation_edits]
  if (length(element_edits) || !is.null(capture)) {
    layout <- apply_elements(layout, built, element_edits, capture,
                             connect_labels = isTRUE(project$connect_labels), previous = previous)
  }
  size <- plot_size(project)
  if (all(size == 100)) return(layout)
  background <- layout$grobs[[match("background", layout$layout$name)]]
  fill <- background$gp$fill
  canvas <- if (length(fill)) grid::rectGrob(gp = grid::gpar(fill = fill, col = NA))
  grid::grobTree(canvas, grid::grobTree(layout,
    vp = grid::viewport(width = size[["width"]] / 100, height = size[["height"]] / 100)))
}

apply_saved_title_offset <- function(layout, project) {
  idx <- which(layout$layout$name == "title")
  if (length(idx) != 1L) stop("A unique plot title is required.")
  title <- layout$grobs[[idx]]
  k <- which(vapply(title$children, inherits, logical(1), "text"))
  if (length(k) != 1L) stop("Add a standard text title before nudging it.")
  label <- title$children[[k]]
  label$x <- label$x + grid::unit(project$dx, "mm")
  label$y <- label$y + grid::unit(project$dy, "mm")
  title$children[[k]] <- label
  layout$grobs[[idx]] <- title
  layout
}

export_figure <- function(project, filename, dpi = 300) {
  device <- NULL
  if (tolower(tools::file_ext(filename)) == "pdf" && capabilities("cairo")) {
    plots <- if (inherits(project, "figure_composition")) project$plots else list(project)
    layers <- unlist(lapply(plots, function(item) item$plot$layers), recursive = FALSE)
    named_fonts <- vapply(layers, function(layer) {
      families <- c(layer$geom_params$annotation$text_family,
                    unlist(lapply(layer$figure_facet_styles, `[[`, "text_family"), use.names = FALSE),
                    unlist(lapply(layer$geom$figure_group_styles, `[[`, "family"), use.names = FALSE))
      any(!families %in% c("", "sans", "serif", "mono"))
    }, logical(1))
    if (any(named_fonts)) device <- grDevices::cairo_pdf
  }
  ggplot2::ggsave(filename, plot = render_figure(project), device = device,
                  width = project$width, height = project$height,
                  units = "mm", dpi = dpi)
  invisible(filename)
}
