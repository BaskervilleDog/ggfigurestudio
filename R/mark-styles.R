mark_fields <- function(layer) {
  geom <- layer$geom
  fields <- c("colour", "fill", "size", "shape", "stroke", "alpha", "linewidth",
              "linetype", "family", "fontface", "angle", "hjust", "vjust", "lineheight")
  outlier_shape <- layer$geom_params$outlier_gp$shape
  if (inherits(geom, "GeomBoxplot") && length(outlier_shape) == 1L && is.na(outlier_shape))
    fields <- setdiff(fields, c("size", "shape"))
  intersect(fields, geom$aesthetics())
}

mark_kind <- function(geom) {
  if (inherits(geom, "GeomPoint")) return("Point")
  if (inherits(geom, c("GeomLine", "GeomPath", "GeomStep"))) return("Line")
  if (inherits(geom, "GeomSegment") && !inherits(geom, "GeomCurve")) return("Segment")
  if (inherits(geom, "GeomVline")) return("Vertical line")
  if (inherits(geom, "GeomHline")) return("Horizontal line")
  if (inherits(geom, "GeomTile")) return("Tile")
  if (inherits(geom, c("GeomBar", "GeomCol"))) return("Bar segment")
  if (inherits(geom, "GeomSf")) return("Area")
  if (inherits(geom, "GeomFigureShape")) return("Shape")
  if (inherits(geom, "GeomViolin")) return("Violin")
  if (inherits(geom, "GeomDensity")) return("Density")
  if (inherits(geom, "GeomBoxplot")) return("Boxplot")
  if (inherits(geom, "GeomRibbon")) return("Ribbon")
  if (inherits(geom, "GeomLabel")) return("Boxed label")
  if (inherits(geom, "GeomText")) return("Text")
  NULL
}

mark_fontface_name <- function(value) {
  faces <- c(unname(editor_font_faces), "symbol")
  value <- as.character(value)
  codes <- match(value, as.character(seq_along(faces)))
  value[!is.na(codes)] <- faces[codes[!is.na(codes)]]
  value
}

style_column <- function(column, rows, value, field) {
  if (field == "fontface") {
    column <- mark_fontface_name(column)
    value <- mark_fontface_name(value)
  }
  if (field == "linetype" && (is.character(column) || is.character(value))) {
    codes <- as.character(0:6)
    types <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    positions <- match(as.character(column), codes)
    column <- as.character(column)
    column[!is.na(positions)] <- types[positions[!is.na(positions)]]
    pos <- match(as.character(value), codes)
    if (!is.na(pos)) value <- types[pos]
  }
  column[rows] <- value
  column
}

style_groups <- function(data, styles) {
  for (group in names(styles)) {
    rows <- which(data$group == as.integer(group))
    if (!length(rows)) next
    for (name in names(styles[[group]])) {
      data[[name]] <- style_column(data[[name]], rows, styles[[group]][[name]], name)
    }
  }
  data
}

mark_geom <- function(source, styles = NULL, layer = NULL, capture = NULL,
                      tile_styles = NULL, tile_layer_style = NULL, reference = NULL) {
  base <- if (is.null(source$figure_base_geom)) source else source$figure_base_geom
  kind <- mark_kind(base)
  line_axis <- reference_axis(base)
  filled <- kind %in% c("Ribbon", "Violin", "Density", "Boxplot")
  force(layer)
  force(styles)
  force(tile_styles)
  force(tile_layer_style)
  force(reference)
  ggplot2::ggproto(NULL, base, figure_base_geom = base, figure_group_styles = styles,
    figure_tile_styles = tile_styles, figure_tile_layer_style = tile_layer_style,
    draw_layer = function(data, params, layout, coord) {
      data <- style_groups(data, styles)
      for (row in which(lengths(data$.figure_facet_style) > 0L)) {
        style <- data$.figure_facet_style[[row]]
        for (field in names(style))
          data[[field]] <- style_column(data[[field]], row, style[[field]], field)
      }
      cells <- if (nrow(data) && (!is.null(capture) || length(tile_styles) || length(tile_layer_style))) {
        if (kind %in% c("Tile", "Bar segment")) tile_cells(data, layout, bar = kind == "Bar segment")
        else if (kind == "Area") {
          labels <- if (!is.null(capture)) capture$area_labels[[as.character(layer)]]
          area_cells(data, layout, labels)
        }
      }
      cell_rows <- which(!is.na(cells$ids))
      for (field in names(tile_layer_style)) {
        data[[field]] <- style_column(data[[field]], cell_rows, tile_layer_style[[field]], field)
      }
      if (length(tile_styles)) data <- style_tiles(data, cells$ids, tile_styles)
      grobs <- base$draw_layer(data, params, layout, coord)
      if (!nrow(data) || (is.null(capture) && is.null(reference)) ||
          (kind != "Area" && is.null(reference) && !coord$is_linear())) return(grobs)
      panel_suffixes <- if (length(reference)) annotation_panel_keys(layout, "")
      move_axis <- line_axis
      if (length(move_axis) && inherits(coord, "CoordFlip"))
        move_axis <- if (move_axis == "x") "y" else "x"
      if (!is.null(cells)) {
        fields <- capture$fields[[as.character(layer)]]
        capture$tile_layer_values[[as.character(layer)]] <-
          lapply(data[cell_rows, intersect(fields, names(data)), drop = FALSE], unique)
        data$.figure_tile_id <- cells$ids
        data$.figure_tile_label <- cells$labels
      }
      panels <- split(data, data$PANEL)
      for (i in seq_along(grobs)) {
        panel <- panels[[i]]
        if (is.null(panel) || !nrow(panel)) next
        panel_id <- as.character(panel$PANEL[1L])
        panel_suffix <- panel_suffixes[[panel_id]]
        reference_key <- if (length(reference)) {
          key <- paste0(reference$key, panel_suffix)
          offset <- reference$offset
          panel_offset <- reference$panel_offsets[[key]]
          if (length(panel_offset)) {
            if (is.null(offset)) offset <- c(0, 0)
            offset <- offset + panel_offset
          }
          grobs[[i]] <- shift_reference_line(grobs[[i]], offset)
          key
        }
        if (is.null(capture)) next
        shapes <- if (filled) group_shapes(grobs[[i]], panel, boxplot = kind == "Boxplot")
        panel_params <- layout$panel_params[[as.integer(panel_id)]]
        drawn_line <- if (length(reference_key) && inherits(grobs[[i]], c("segments", "polyline")))
          grobs[[i]]
        if (length(reference_key) && is.null(drawn_line)) next
        if (length(line_axis) && is.null(drawn_line))
          panel <- reference_segment_coords(panel, panel_params, coord, line_axis)
        coords <- if (filled || !is.null(drawn_line)) panel else coord$transform(panel, panel_params)
        probe <- grid::grob(coords = coords, kind = kind, layer = layer, shapes = shapes, filled = filled,
                            segment = kind == "Segment" || length(line_axis) > 0L,
                            drawn_line = drawn_line, reference_key = reference_key, panel_suffix = panel_suffix,
                            move_axis = move_axis,
                            capture = capture, cl = "figure_mark_probe")
        grobs[[i]] <- grid::gTree(children = grid::gList(grobs[[i]], probe),
                                  name = grobs[[i]]$name, cl = "figure_mark_wrapper")
      }
      grobs
    })
}

styled_plot <- function(plot, layer, group, styles, cell = NULL, all_cells = FALSE, changed_fields = character()) {
  item <- plot$layers[[layer]]
  copy <- rlang::env_clone(item)
  attributes(copy) <- attributes(item)
  all_styles <- item$geom$figure_group_styles
  tile_styles <- item$geom$figure_tile_styles
  tile_layer_style <- item$geom$figure_tile_layer_style
  if (all_cells) {
    tile_layer_style <- styles
    # A layer edit takes precedence for the fields the user just changed.
    tile_styles <- Filter(length, lapply(tile_styles, function(style) {
      style[setdiff(names(style), changed_fields)]
    }))
  } else if (is.null(cell)) {
    all_styles[[group]] <- styles
  } else {
    tile_styles[[cell]] <- styles
  }
  copy$geom <- mark_geom(item$geom, all_styles, tile_styles = tile_styles,
                        tile_layer_style = tile_layer_style)
  plot$layers[[layer]] <- copy
  with_group_style_guides(plot)
}

capture_mark_layers <- function(built, capture, edits = NULL, connect_labels = FALSE, previous = NULL) {
  plot <- built$plot
  # Keep the data and scales trained by ggplot2. Attach probes only for drawing.
  for (i in seq_along(plot$layers)) {
    item <- plot$layers[[i]]
    kind <- mark_kind(item$geom)
    if (is.null(kind)) next
    if (kind == "Shape") {
      key <- shape_layer_key(item, i)
      move_key <- paste0("element/", key)
      offset <- edits[[move_key]]
      panel_offsets <- edits[startsWith(as.character(names(edits)), annotation_panel_prefix(move_key))]
      if (is.null(capture) && is.null(offset) && !length(panel_offsets)) next
      copy <- rlang::env_clone(item)
      attributes(copy) <- attributes(item)
      copy$geom <- ggplot2::ggproto(NULL, GeomFigureShape, figure_hit_capture = capture, figure_hit_layer = i,
                                    figure_shape_key = key, figure_shape_offset = offset,
                                    figure_connect_labels = connect_labels,
                                    figure_previous_label_offsets = previous,
                                    figure_shape_panels = annotation_panel_keys(built$layout, ""),
                                    figure_shape_panel_offsets = panel_offsets,
                                    figure_shape_panel_anchors = item$figure_facet_positions)
      plot$layers[[i]] <- copy
      next
    }
    reference_info <- reference_line_info(item)
    reference <- if (length(reference_info)) {
      key <- reference_move_key(item, i)
      list(key = key, offset = edits[[key]],
           panel_offsets = edits[startsWith(as.character(names(edits)), annotation_panel_prefix(key))])
    }
    if (is.null(capture) && is.null(reference$offset) && !length(reference$panel_offsets) &&
        !length(item$figure_facet_styles)) next
    copy <- rlang::env_clone(item)
    attributes(copy) <- attributes(item)
    copy$geom <- mark_geom(item$geom, item$geom$figure_group_styles, i, capture,
                           tile_styles = item$geom$figure_tile_styles,
                           tile_layer_style = item$geom$figure_tile_layer_style,
                           reference = reference)
    plot$layers[[i]] <- copy
    if (is.null(capture)) next
    capture$palette_targets[[as.character(i)]] <- point_palette_targets(plot, item)
    capture$tile_palette_type[[as.character(i)]] <- tile_palette_type(plot, item)
    fields <- mark_fields(item)
    if (kind == "Area") {
      fields <- intersect(fields, c("fill", "colour", "linewidth", "linetype", "alpha"))
      capture$area_labels[[as.character(i)]] <- area_feature_labels(plot, item, built$data[[i]])
    }
    capture$fields[[as.character(i)]] <- fields
    capture$reference_lines[[as.character(i)]] <- reference_info
    if (length(reference_info))
      capture$reference_positions[[as.character(i)]] <- item$figure_facet_positions
    capture$group_names[[as.character(i)]] <- display_groups(built, i)
  }
  built$plot <- plot
  built
}

display_groups <- function(built, layer) {
  data <- built$data[[layer]]
  groups <- unique(data$group)
  labels <- stats::setNames(paste("Group", groups), groups)
  if (length(groups) == 1L) labels[] <- "Only group"
  categories <- stats::setNames(vector("list", length(groups)), groups)
  item <- built$plot$layers[[layer]]
  mapped_aes <- setdiff(names(item$computed_mapping), names(item$aes_params))
  candidates <- intersect(c("colour", "fill", "linetype", "shape"), intersect(names(data), mapped_aes))
  if (identical(mark_kind(item$geom), "Segment")) {
    candidates <- c(candidates, intersect(c("x", "y"), mapped_aes))
  }
  for (aes in candidates) {
    if (aes %in% c("x", "y")) {
      scales <- built$layout[[paste0("panel_scales_", aes)]]
      scale <- if (length(scales) == 1L) scales[[1L]]
    } else {
      scale <- built$plot$scales$get_scales(aes)
    }
    if (is.null(scale) || !scale$is_discrete()) next
    breaks <- scale$get_breaks()
    mapped <- scale$map(breaks)
    for (group in groups) {
      values <- unique(data[[aes]][data$group == group])
      if (length(values) != 1L) next
      idx <- which(mapped == values)
      if (length(idx) == 1L) {
        key <- as.character(group)
        categories[[key]] <- c(categories[[key]], as.character(breaks[idx]))
      }
    }
  }
  matched <- lengths(categories) > 0L
  labels[matched] <- vapply(categories[matched], function(x) paste(unique(x), collapse = " / "), character(1))
  labels
}

# Use the geometry ggplot2 drew, including any gaps and orientation changes.
group_shapes <- function(grob, data, boxplot) {
  groups <- sort(unique(data$group))
  if (length(grob$children) != length(groups)) return(list())
  classes <- if (boxplot) c("polygon", "segments") else "polygon"
  collect <- function(child) {
    if (inherits(child, classes)) return(list(child))
    unlist(lapply(child$children, collect), recursive = FALSE, use.names = FALSE)
  }
  stats::setNames(lapply(grob$children, collect), groups)
}

segment_hit <- function(grob, key, bounds, radius = 1) {
  start <- grid::deviceLoc(grob$x0, grob$y0, valueOnly = TRUE)
  end <- grid::deviceLoc(grob$x1, grob$y1, valueOnly = TRUE)
  interleave <- function(a, b) as.vector(rbind(a, b, NA_real_))
  list(key = key, x = interleave(start$x, end$x), y = interleave(start$y, end$y),
       bounds = bounds, line = TRUE, radius = radius)
}

polygon_hit <- function(grob, key, bounds) {
  if (is.null(grob) || !length(grob$x)) return(NULL)
  pos <- grid::deviceLoc(grob$x, grob$y, valueOnly = TRUE)
  id <- grob$id
  if (!is.null(grob$id.lengths)) id <- rep(seq_along(grob$id.lengths), grob$id.lengths)
  if (is.null(id)) id <- rep(1L, length(pos$x))
  pieces <- split(seq_along(pos$x), id)
  pieces <- lapply(pieces, function(rows) rows[is.finite(pos$x[rows]) & is.finite(pos$y[rows])])
  list(key = key, x = pos$x, y = pos$y, bounds = bounds,
       polygons = Filter(function(rows) length(rows) >= 3L, pieces))
}

drawDetails.figure_mark_probe <- function(x, recording) {
  data <- x$coords
  limits <- grid::deviceLoc(grid::unit(0:1, "npc"), grid::unit(0:1, "npc"), valueOnly = TRUE)
  bounds <- c(limits$x, limits$y)
  if (x$kind %in% c("Tile", "Bar segment")) return(invisible(capture_tiles(x, bounds)))
  if (x$kind == "Area") return(invisible(capture_areas(x, bounds)))
  pos <- if (!x$filled && !x$segment)
    grid::deviceLoc(grid::unit(data$x, "native"), grid::unit(data$y, "native"), valueOnly = TRUE)
  for (group in unique(data$group)) {
    rows <- which(data$group == group)
    key <- paste0(paste("mark", x$layer, group, sep = "/"), x$panel_suffix)
    if (is.null(x$capture$marks[[key]])) {
      fields <- x$capture$fields[[as.character(x$layer)]]
      values <- lapply(data[rows, intersect(fields, names(data)), drop = FALSE], unique)
      title <- unname(x$capture$group_names[[as.character(x$layer)]][as.character(group)])
      if (!length(title) || is.na(title)) title <- paste("Group", group)
      reference <- x$capture$reference_lines[[as.character(x$layer)]]
      if (length(reference) && length(x$panel_suffix)) {
        positions <- x$capture$reference_positions[[as.character(x$layer)]][[x$panel_suffix]]
        if (length(positions[[reference$axis]])) reference$value <- positions[[reference$axis]]
      }
      x$capture$marks[[key]] <- list(layer = x$layer, group = as.character(group), kind = x$kind,
        title = paste(x$kind, "-", title, "(layer", paste0(x$layer, ")")), values = values,
        palette_targets = x$capture$palette_targets[[as.character(x$layer)]],
        reference = reference, faceted = length(x$panel_suffix) > 0L, panel_suffix = x$panel_suffix,
        move_key = x$reference_key)
    }
    # Prefer the measured label buttons to a hit at the label's data anchor.
    # An anchor hit could cover a separate point even when the text sits above it.
    if (x$kind %in% c("Text", "Boxed label") && key %in% x$capture$links) next
    if (x$filled) {
      for (shape in x$shapes[[as.character(group)]]) {
        path <- if (inherits(shape, "segments")) segment_hit(shape, key, bounds) else
          polygon_hit(shape, key, bounds)
        if (!is.null(path)) {
          # Dense jittered points can cover the entire box's click area.
          path$priority <- x$kind == "Boxplot" && inherits(shape, "polygon")
          x$capture$paths[[length(x$capture$paths) + 1L]] <- path
        }
      }
    } else if (!is.null(x$drawn_line)) {
      path <- reference_hit(x$drawn_line, key, bounds, x$reference_key, x$move_axis)
      x$capture$marks[[key]]$visible <- any(is.finite(path$x) & is.finite(path$y)) &&
        any(path$x >= bounds[1L], na.rm = TRUE) && any(path$x <= bounds[2L], na.rm = TRUE) &&
        any(path$y >= bounds[3L], na.rm = TRUE) && any(path$y <= bounds[4L], na.rm = TRUE)
      x$capture$paths[[length(x$capture$paths) + 1L]] <- path
    } else if (x$segment) {
      segments <- grid::segmentsGrob(data$x[rows], data$y[rows],
        data$xend[rows], data$yend[rows], default.units = "native")
      x$capture$paths[[length(x$capture$paths) + 1L]] <- segment_hit(segments, key, bounds, radius = 1.4)
    } else {
      x$capture$paths[[length(x$capture$paths) + 1L]] <-
        list(key = key, x = pos$x[rows], y = pos$y[rows], bounds = bounds,
             line = x$kind == "Line",
             radius = if (x$kind == "Point") pmax(1.3, data$size[rows] / 2) else 2)
    }
  }
}

drawDetails.figure_grid_hit <- function(x, recording) {
  NextMethod()
  pos <- grid::deviceLoc(x$x, x$y, valueOnly = TRUE)
  for (i in seq(1L, length(pos$x), by = 2L)) {
    rows <- i:(i + 1L)
    x$capture$paths[[length(x$capture$paths) + 1L]] <- list(
      key = x$key, x = pos$x[rows], y = pos$y[rows], line = TRUE, radius = 1)
  }
}
