tile_axis_values <- function(scale, values) {
  if (!scale$is_discrete()) return(as.character(scale$get_transformation()$inverse(values)))
  levels <- scale$get_limits()
  position <- match(as.numeric(values), as.numeric(scale$map(levels)))
  label <- as.character(values)
  label[!is.na(position)] <- as.character(levels[position[!is.na(position)]])
  label
}

tile_cells <- function(data, layout, bar = FALSE) {
  ids <- labels <- character(nrow(data))
  info <- layout$layout
  variables <- intersect(facet_variables(layout$facet_params), names(info))
  for (panel in unique(data$PANEL)) {
    rows <- which(data$PANEL == panel)
    entry <- match(panel, info$PANEL)
    scales <- layout$get_scales(entry)
    panel_id <- facet_id(info[entry, variables, drop = FALSE])
    if (bar) {
      axis <- if (isTRUE(data$flipped_aes[rows[1L]])) "y" else "x"
      position <- tile_axis_values(scales[[axis]], data[[axis]][rows])
      ids[rows] <- paste(panel_id, vapply(position, utils::URLencode, character(1), reserved = TRUE),
                         data$group[rows], sep = "/")
      labels[rows] <- position
    } else {
      x <- tile_axis_values(scales$x, data$x[rows])
      y <- tile_axis_values(scales$y, data$y[rows])
      ids[rows] <- paste(panel_id, vapply(x, utils::URLencode, character(1), reserved = TRUE),
                         vapply(y, utils::URLencode, character(1), reserved = TRUE), sep = "/")
      labels[rows] <- paste0("x = ", x, ", y = ", y)
    }
  }
  list(ids = make.unique(ids, sep = ":"), labels = labels)
}

style_tiles <- function(data, cells, styles) {
  for (cell in intersect(cells, names(styles))) {
    row <- match(cell, cells)
    for (field in names(styles[[cell]])) {
      data[[field]] <- style_column(data[[field]], row, styles[[cell]][[field]], field)
    }
  }
  data
}

capture_tiles <- function(probe, bounds) {
  data <- probe$coords
  lo <- grid::deviceLoc(grid::unit(data$xmin, "native"), grid::unit(data$ymin, "native"),
                         valueOnly = TRUE)
  hi <- grid::deviceLoc(grid::unit(data$xmax, "native"), grid::unit(data$ymax, "native"),
                         valueOnly = TRUE)
  fields <- intersect(probe$capture$fields[[as.character(probe$layer)]], names(data))
  bar <- probe$kind == "Bar segment"
  groups <- if (bar) probe$capture$group_names[[as.character(probe$layer)]]
  for (row in which(is.finite(lo$x) & is.finite(lo$y) & is.finite(hi$x) & is.finite(hi$y))) {
    if (lo$x[row] == hi$x[row] || lo$y[row] == hi$y[row]) next
    cell <- data$.figure_tile_id[row]
    key <- paste(if (bar) "bar" else "tile", probe$layer, cell, sep = "/")
    label <- data$.figure_tile_label[row]
    group <- if (bar) unname(groups[as.character(data$group[row])])
    if (length(group) && !is.na(group) && group != label && group != "Only group")
      label <- paste(label, group, sep = " / ")
    probe$capture$marks[[key]] <- list(layer = probe$layer, cell = cell, kind = probe$kind,
      title = paste(probe$kind, "-", label),
      values = as.list(data[row, fields, drop = FALSE]),
      layer_values = probe$capture$tile_layer_values[[as.character(probe$layer)]],
      palette_type = probe$capture$tile_palette_type[[as.character(probe$layer)]])
    probe$capture$paths[[length(probe$capture$paths) + 1L]] <- list(
      key = key, x = c(lo$x[row], hi$x[row], hi$x[row], lo$x[row]),
      y = c(lo$y[row], lo$y[row], hi$y[row], hi$y[row]),
      bounds = bounds, polygons = list(1:4))
  }
}
