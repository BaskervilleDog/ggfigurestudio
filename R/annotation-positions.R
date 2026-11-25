set_facet_annotation_position <- function(project, mark, positions, panels = mark$panel_suffix) {
  layer <- project$plot$layers[[mark$layer]]
  copy <- rlang::env_clone(layer)
  attributes(copy) <- attributes(layer)
  anchors <- layer$figure_facet_positions
  # An explicit axis position replaces any physical nudge on the edited axis.
  base_key <- sub("/facet/.*$", "", mark$move_key)
  base_offset <- project$element_edits[[base_key]]
  if (is.null(base_offset)) base_offset <- c(0, 0)
  axes <- match(names(positions), c("x", "y"))
  if (inherits(project$plot$coordinates, "CoordFlip")) axes <- 3L - axes
  for (panel in panels) {
    anchors[[panel]][names(positions)] <- positions
    key <- paste0(base_key, panel)
    offset <- project$element_edits[[key]]
    if (is.null(offset)) offset <- c(0, 0)
    offset[axes] <- -base_offset[axes]
    project$element_edits[[key]] <- if (any(offset != 0)) offset
  }
  copy$figure_facet_positions <- anchors
  project$plot$layers[[mark$layer]] <- copy
  project
}

set_facet_annotation_style <- function(plot, mark, changes, all_facets = FALSE, reset = FALSE,
                                       panels = mark$panel_suffix) {
  if (!reset && !length(changes)) return(plot)
  layer <- plot$layers[[mark$layer]]
  copy <- rlang::env_clone(layer)
  attributes(copy) <- attributes(layer)
  styles <- layer$figure_facet_styles
  if (all_facets) {
    styles <- if (!reset) Filter(length, lapply(styles, function(style) {
      style[setdiff(names(style), names(changes))]
    }))
  } else {
    for (panel in panels) {
      if (reset) styles[[panel]] <- NULL
      else styles[[panel]][names(changes)] <- changes
    }
  }
  copy$figure_facet_styles <- styles
  plot$layers[[mark$layer]] <- copy
  plot
}

apply_facet_annotation_edits <- function(built) {
  layout <- built$layout
  for (i in seq_along(built$plot$layers)) {
    layer <- built$plot$layers[[i]]
    anchors <- layer$figure_facet_positions
    styles <- layer$figure_facet_styles
    if (!length(anchors) && !length(styles)) next
    panels <- annotation_panel_keys(layout, "")
    data <- built$data[[i]]
    if (length(styles)) data$.figure_facet_style <- vector("list", nrow(data))
    for (panel in intersect(as.character(unique(data$PANEL)), names(panels))) {
      key <- panels[[panel]]
      rows <- which(data$PANEL == panel)
      style <- styles[[key]]
      if (length(style)) data$.figure_facet_style[rows] <- rep(list(style), length(rows))
      positions <- anchors[[key]]
      if (!length(positions)) next
      scales <- layout$get_scales(match(panel, as.character(layout$layout$PANEL)))
      for (axis in names(positions)) {
        scale <- scales[[axis]]
        value <- parse_axis_position(scale, positions[[axis]])
        # Built layer data uses transformed positions. Discrete categories are
        # already mapped to panel positions by parse_axis_position().
        if (!is.null(scale) && !scale$is_discrete()) value <- scale$transform(value)
        if (!is.finite(value)) stop("Enter a position within the facet's axis scale.")
        aesthetic <- if (length(reference_axis(layer$geom))) paste0(axis, "intercept") else axis
        data[[aesthetic]][rows] <- as.numeric(value)
      }
    }
    built$data[[i]] <- data
  }
  built
}

