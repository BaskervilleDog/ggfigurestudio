facet_variables <- function(params) {
  unique(c(names(params$facets), names(params$rows), names(params$cols)))
}

facet_id <- function(row) {
  as.character(jsonlite::toJSON(lapply(row[sort(names(row))], as.character), auto_unbox = TRUE))
}

annotation_panel_prefix <- function(key) paste0(key, "/facet/")

annotation_panel_keys <- function(layout, key) {
  panels <- layout$layout
  variables <- intersect(facet_variables(layout$facet_params), names(panels))
  if (!length(variables)) {
    if (nrow(panels) <= 1L) return(NULL)
    variables <- "PANEL"
  }
  keys <- vapply(seq_len(nrow(panels)), function(i) {
    id <- facet_id(panels[i, variables, drop = FALSE])
    paste0(annotation_panel_prefix(key), utils::URLencode(id, reserved = TRUE, repeated = TRUE))
  }, character(1))
  stats::setNames(as.list(keys), as.character(panels$PANEL))
}

annotation_facet_choices <- function(layout) {
  keys <- annotation_panel_keys(layout, "")
  if (!length(keys)) return(character())
  panels <- layout$layout
  variables <- intersect(facet_variables(layout$facet_params), names(panels))
  labels <- vapply(seq_len(nrow(panels)), function(i) {
    if (!length(variables)) return(paste("Panel", panels$PANEL[i]))
    row <- panels[i, variables, drop = FALSE]
    paste(paste(names(row), vapply(row, as.character, character(1)), sep = " = "), collapse = ", ")
  }, character(1))
  stats::setNames(unlist(keys, use.names = FALSE), labels)
}

scope_annotation_facets <- function(plot) {
  restricted <- vapply(plot$layers, function(layer) length(attr(layer$layout, "figure_facets")) > 0L, logical(1))
  if (!any(restricted)) return(plot)
  base <- plot$facet
  plot$facet <- ggplot2::ggproto(NULL, base, map_data = function(data, layout, params) {
    targets <- attr(attr(data, "layout"), "figure_facets")
    if (length(targets)) {
      keys <- annotation_panel_keys(list(layout = layout, facet_params = params), "")
      if (length(keys)) {
        panels <- which(unlist(keys, use.names = FALSE) %in% targets)
        attr(data, "layout") <- if (length(panels)) as.integer(layout$PANEL[panels]) else 0L
      }
    }
    base$map_data(data, layout, params)
  })
  plot
}

facet_cell_panel <- function(cell, panels, info, side) {
  i <- axis_panel(cell, panels, side)
  if (!length(i)) return(integer())
  row <- match(panels$t[i], sort(unique(panels$t)))
  col <- match(panels$l[i], sort(unique(panels$l)))
  which(info$ROW == row & info$COL == col)
}

facet_strip_rows <- function(layout, params, part) {
  vars <- switch(part, facets = names(params$facets), rows = names(params$rows),
                 cols = names(params$cols))
  rows <- layout[intersect(vars, names(layout))]
  if (part != "facets") rows <- unique(rows)
  rows
}

supported_facet <- function(facet) inherits(facet, c("FacetWrap", "FacetGrid"))

new_facet_settings <- function(facet, titles, breaks) {
  base <- facet$figureedit_base
  if (is.null(base)) base <- facet
  params <- base$params
  for (axis in c("x", "y")) {
    if (length(breaks[[axis]])) {
      params$draw_axes[[axis]] <- TRUE
      params$axis_labels[[axis]] <- TRUE
    }
  }
  ggplot2::ggproto(NULL, base, params = params, figureedit_base = base,
    figureedit_titles = titles, figureedit_breaks = breaks,
    attach_axes = function(table, layout, ranges, coord, theme, params) {
      if (!any(lengths(breaks) > 0L))
        return(base$attach_axes(table, layout, ranges, coord, theme, params))
      # Draw from each panel's guide even when panels share a trained scale.
      layout$SCALE_X <- layout$SCALE_Y <- as.integer(layout$PANEL)
      ggplot2::FacetWrap$attach_axes(table, layout, ranges, coord, theme, params)
    },
    format_strip_labels = function(layout, params) {
      labels <- base$format_strip_labels(layout, params)
      for (part in intersect(names(titles), names(labels))) {
        rows <- facet_strip_rows(layout, params, part)
        for (i in seq_len(nrow(rows))) {
          changes <- titles[[part]][[facet_id(rows[i, , drop = FALSE])]]
          for (column in names(changes))
            labels[[part]][[as.integer(column)]][i] <- changes[[column]]
        }
      }
      labels
    })
}

facet_tick_layout <- function(layout) {
  if (isTRUE(layout$figureedit_facet_ticks)) return(layout)
  ggplot2::ggproto(NULL, layout, figureedit_facet_ticks = TRUE,
    setup_panel_guides = function(self, guides, layers) {
      breaks <- self$facet$figureedit_breaks
      if (any(lengths(breaks) > 0L) && inherits(self$coord, "CoordCartesian") &&
          !inherits(self$coord, "CoordFlip")) {
        info <- self$layout
        vars <- intersect(facet_variables(self$facet_params), names(info))
        panels <- self$panel_params
        for (i in seq_along(panels)) {
          key <- facet_id(info[i, vars, drop = FALSE])
          for (axis in names(breaks)) {
            values <- breaks[[axis]][[key]]
            if (!is.null(values))
              panels[[i]] <- replace_panel_breaks(panels[[i]], axis, values)
          }
        }
        self$panel_params <- panels
        # Fixed scales may share a coordinate index. Train guides separately
        # now that otherwise identical panels can have different tick breaks.
        info$COORD <- seq_len(nrow(info))
        self$layout <- info
      }
      ggplot2::ggproto_parent(layout, self)$setup_panel_guides(guides, layers)
    })
}

replace_panel_breaks <- function(panel, axis, values) {
  values <- as.numeric(values)
  view <- panel[[axis]]
  scale <- ggplot2::ggproto(NULL, view$scale, breaks = values)
  # Preserve the original label formatter when it can format arbitrary breaks.
  if (is.atomic(scale$labels) && !is.null(scale$labels)) scale$labels <- ggplot2::waiver()
  major <- suppressWarnings(scale$get_transformation()$transform(values))
  if (any(!is.finite(major))) stop("One or more ticks are outside this scale's transformation domain.")
  original <- view$continuous_range
  limits <- range(c(original, major))
  if (original[1L] > original[2L]) limits <- rev(limits)
  minor <- scale$get_breaks_minor(b = major, limits = sort(limits))
  panel[[axis]] <- ggplot2::ggproto(NULL, view, scale = scale, breaks = major,
                                    minor_breaks = minor, continuous_range = limits)
  panel[[paste0(axis, ".range")]] <- limits

  # A secondary guide uses the same physical viewport as the primary axis.
  secondary_name <- paste0(axis, ".sec")
  secondary <- panel[[secondary_name]]
  if (!is.null(secondary$break_info) && !identical(original, limits)) {
    secondary_axis <- ggplot2::ggproto(NULL, scale$secondary.axis)
    secondary_axis$init(scale)
    info <- secondary_axis$break_info(limits, scale)
    names(info) <- sub("^sec[.]", "", names(info))
    panel[[secondary_name]] <- ggplot2::ggproto(NULL, secondary, break_info = info,
      continuous_range = sort(limits), rescale = function(self, x) {
        from <- self$break_info$range
        (x - from[1L]) / diff(from)
      })
  }
  panel
}

set_facet_title <- function(plot, target, value) {
  facet <- plot$facet
  titles <- facet$figureedit_titles
  titles[[target$part]][[target$id]][[as.character(target$column)]] <- value
  plot$facet <- new_facet_settings(facet, titles, facet$figureedit_breaks)
  plot
}

set_facet_breaks <- function(plot, id, values) {
  facet <- plot$facet
  breaks <- facet$figureedit_breaks
  for (axis in names(values)) breaks[[axis]][[id]] <- values[[axis]]
  if (inherits(facet, "FacetNull")) {
    base <- if (is.null(facet$figureedit_base)) facet else facet$figureedit_base
    plot$facet <- ggplot2::ggproto(NULL, base, figureedit_base = base, figureedit_breaks = breaks)
  } else {
    plot$facet <- new_facet_settings(facet, facet$figureedit_titles, breaks)
  }
  plot$layout <- facet_tick_layout(plot$layout)
  plot
}

facet_tick_target <- function(built, panel, axes = c("x", "y")) {
  facet <- built$layout$facet
  single_panel <- inherits(facet, "FacetNull")
  if ((!supported_facet(facet) && !single_panel) || length(panel) != 1L ||
      !inherits(built$layout$coord, "CoordCartesian") || inherits(built$layout$coord, "CoordFlip"))
    return(NULL)
  info <- built$layout$layout
  vars <- intersect(facet_variables(facet$params), names(info))
  row <- info[panel, vars, drop = FALSE]
  if (!length(row) && !single_panel) return(NULL)
  id <- facet_id(row)
  views <- built$layout$panel_params[[as.integer(info$PANEL[panel])]]
  values <- list()
  for (axis in axes) {
    view <- views[[axis]]
    if (is.null(view$scale) || view$scale$is_discrete()) next
    position <- view$scale$get_transformation()$inverse(view$breaks)
    if (!is.numeric(position)) next
    values[axis] <- list(facet$figureedit_breaks[[axis]][[id]])
  }
  if (!length(values)) return(NULL)
  label <- if (single_panel) NULL else
    paste(paste(names(row), vapply(row, as.character, character(1)), sep = " = "), collapse = ", ")
  list(id = id, values = values, label = label)
}

facet_break_inputs <- function(target) {
  if (is.null(target)) return(NULL)
  editor_group(if (length(target$label)) paste("Tick positions for", target$label) else "Tick positions",
    lapply(names(target$values), function(axis) {
      values <- target$values[[axis]]
      text <- attr(values, "figureedit_input")
      if (is.null(text)) text <- paste(values, collapse = ", ")
      shiny::textInput(paste0("facet_breaks_", axis), paste(toupper(axis), "tick positions"),
        text, placeholder = "Automatic")
    }),
    shiny::p(class = "editor-help", "Enter values such as 0, 100, 200 or a range such as 0 to 1100 by 100. Leave blank to restore this panel's original ticks and range."),
    shiny::actionButton("apply_facet_breaks", if (length(target$label)) "Apply facet ticks" else "Apply ticks"))
}

parse_facet_breaks <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) return(NULL)
  pattern <- "^([^[:space:]]+)[[:space:]]+to[[:space:]]+([^[:space:]]+)[[:space:]]+by[[:space:]]+([^[:space:]]+)$"
  range_parts <- regmatches(text, regexec(pattern, text, ignore.case = TRUE))[[1L]]
  if (length(range_parts)) {
    bounds <- suppressWarnings(as.numeric(range_parts[-1L]))
    from <- bounds[1L]
    to <- bounds[2L]
    step <- bounds[3L]
    if (any(!is.finite(bounds)) || from > to || step <= 0)
      stop("Use a numeric start and end in ascending order and a positive step.")
    if ((to - from) / step >= 10000)
      stop("This range would generate too many ticks. Use a larger step.")
    return(structure(seq(from, to, by = step), figureedit_input = text))
  }
  values <- suppressWarnings(as.numeric(strsplit(text, "[,[:space:]]+")[[1L]]))
  if (any(!is.finite(values))) stop("Enter finite numeric tick positions or a range like 0 to 1100 by 100.")
  sort(unique(values))
}

facet_strip_target <- function(built, cell, panels, originals) {
  facet <- built$layout$facet
  info <- built$layout$layout
  side <- substr(cell$name, 7, 7)
  panel <- facet_cell_panel(cell, panels, info, side)
  if (inherits(facet, "FacetWrap")) {
    part <- "facets"
    index <- panel
  } else {
    part <- if (side %in% c("t", "b")) "cols" else "rows"
    index <- as.integer(sub(".*-", "", cell$name))
  }
  rows <- facet_strip_rows(info, facet$params, part)
  if (length(index) != 1L || is.na(index) || index > nrow(rows)) return(NULL)
  list(part = part, id = facet_id(rows[index, , drop = FALSE]),
       original = originals[[part]][index, , drop = FALSE],
       ticks = if (part == "facets") facet_tick_target(built, panel))
}
