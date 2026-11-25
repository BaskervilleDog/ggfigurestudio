area_cells <- function(data, layout, labels) {
  ids <- names <- rep(NA_character_, nrow(data))
  polygons <- vapply(data$geometry, inherits, logical(1), c("POLYGON", "MULTIPOLYGON"))
  info <- layout$layout
  variables <- intersect(facet_variables(layout$facet_params), names(info))
  for (panel in unique(data$PANEL[polygons])) {
    rows <- which(data$PANEL == panel)
    entry <- match(panel, info$PANEL)
    panel_id <- facet_id(info[entry, variables, drop = FALSE])
    ids[rows] <- paste(panel_id, seq_along(rows), sep = "/")
    names[rows] <- paste("Feature", seq_along(rows))
  }
  ids[!polygons] <- NA_character_
  if (length(labels)) names[!is.na(labels)] <- labels[!is.na(labels)]
  list(ids = ids, labels = names)
}

area_feature_labels <- function(plot, layer, data) {
  source <- if (inherits(layer$data, "waiver")) plot$data else layer$data
  if (!inherits(source, "sf") || nrow(source) != nrow(data)) return(NULL)
  column <- intersect(c("NAMELSAD", "NAME", "name"), names(source))[1L]
  if (is.na(column)) return(NULL)
  geometry <- source[[attr(source, "sf_column")]]
  # A stat, facet or alternate geometry mapping may change the source row order.
  if (!all(vapply(seq_along(geometry), function(i) {
    identical(geometry[[i]], data$geometry[[i]])
  }, logical(1)))) return(NULL)
  labels <- as.character(source[[column]])
  labels[!nzchar(labels)] <- NA_character_
  labels
}

capture_areas <- function(probe, bounds) {
  data <- probe$coords
  layer <- as.character(probe$layer)
  fields <- intersect(probe$capture$fields[[layer]], names(data))
  for (row in which(!is.na(data$.figure_tile_id))) {
    shape <- data$geometry[[row]]
    rings <- if (inherits(shape, "POLYGON")) unclass(shape) else
      unlist(unclass(shape), recursive = FALSE)
    rings <- Filter(function(ring) nrow(ring) >= 3L && all(is.finite(ring[, 1:2])), rings)
    if (!length(rings)) next
    xy <- do.call(rbind, lapply(rings, function(ring) ring[, 1:2, drop = FALSE]))
    pos <- grid::deviceLoc(grid::unit(xy[, 1L], "native"), grid::unit(xy[, 2L], "native"),
                           valueOnly = TRUE)
    cell <- data$.figure_tile_id[row]
    key <- paste("area", layer, cell, sep = "/")
    probe$capture$marks[[key]] <- list(layer = probe$layer, cell = cell, kind = "Area",
      title = paste("Area -", data$.figure_tile_label[row]),
      values = as.list(data[row, fields, drop = FALSE]),
      layer_values = probe$capture$tile_layer_values[[layer]],
      palette_type = probe$capture$tile_palette_type[[layer]])
    sizes <- vapply(rings, nrow, integer(1))
    probe$capture$paths[[length(probe$capture$paths) + 1L]] <- list(
      key = key, x = pos$x, y = pos$y, bounds = bounds, evenodd = TRUE,
      polygons = split(seq_len(nrow(xy)), rep(seq_along(rings), sizes)))
  }
}
