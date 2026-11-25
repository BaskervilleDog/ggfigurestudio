aesthetic_arguments <- function(name) {
  if (grepl("(^|[._])(colou?r|fill)$", name))
    return('R colour name, "#RRGGBB", "#RRGGBBAA", or NA')
  if (grepl("(^|[._])(size|linewidth|stroke|width|height)$", name))
    return("Numeric dimension, usually nonnegative. Example: 1.5")
  if (grepl("(^|[._])linetype$", name))
    return('0-6, "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", or custom dash pattern')
  if (grepl("(^|[._])shape$", name))
    return('Point symbol 0-25, a single character, or a ggplot2 shape name. Shapes 21-25 support fill.')
  if (grepl("(^|[._])alpha$", name)) return("Opacity from 0 to 1, or NA for the default")
  if (grepl("^[xy]($|min|max|end|intercept|lower|upper|middle)", name))
    return("Position on the relevant scale: numeric, discrete, date, or date-time as supported")
  switch(name,
    label = 'Text, number, or expression. Example: "Control"',
    family = 'Installed font family or "sans", "serif", "mono"',
    fontface = '"plain", "bold", "italic", "bold.italic", or numeric font code',
    hjust = 'Numeric alignment (0 = left, 1 = right), "inward", or "outward"',
    vjust = 'Numeric alignment (0 = bottom, 1 = top), "inward", or "outward"',
    angle = "Rotation in degrees, e.g. 45",
    group = "Grouping variable or constant identifier, e.g. 1",
    weight = "Numeric observation weights",
    lineheight = "Numeric line spacing multiplier",
    lower = , upper = , middle = , intercept = , slope = "Numeric value on the layer scale",
    map_id = "Identifier matching a region in the map data",
    subgroup = "Identifier separating rings within a polygon group",
    order = "Ordering value within a group, e.g. an integer rank",
    radius = "Numeric radius for the selected geom",
    z = "Numeric surface height for contour statistics",
    nudge_x = , nudge_y = "Numeric offset in data units",
    sample = "Sample values used by the statistic",
    geometry = "An sf geometry column (sfc)",
    "Values are defined by this geom/stat. Use its documented R values or map a suitable column.")
}

describe_aes <- function(x) {
  if (rlang::is_quosure(x)) x <- rlang::quo_get_expr(x)
  paste(deparse(x, width.cutoff = 80L), collapse = " ")
}

supported_aes <- function(layer) {
  unique(c(layer$geom$aesthetics(), layer$stat$aesthetics(),
           names(layer$mapping), names(layer$aes_params)))
}

list_aesthetics <- function(plot, layer = 0L) {
  stopifnot(inherits(plot, "ggplot"), length(layer) == 1L,
            is.numeric(layer), is.finite(layer), layer == as.integer(layer),
            layer >= 0, layer <= length(plot$layers))
  global <- layer == 0L
  item <- if (!global) plot$layers[[layer]]
  keys <- if (global) unique(c(names(plot$mapping), unlist(lapply(plot$layers, supported_aes))))
          else supported_aes(item)
  keys <- sort(as.character(keys))
  local <- if (global) plot$mapping else item$mapping
  required <- c(item$geom$required_aes, item$stat$required_aes)
  out <- data.frame(aesthetic = keys, source = rep("Unset", length(keys)),
                    value = rep("", length(keys)), required_group = rep("", length(keys)),
                    arguments = vapply(keys, aesthetic_arguments, character(1)),
                    row.names = NULL)
  for (i in seq_along(keys)) {
    key <- keys[i]
    layers <- list("Fixed" = item$aes_params, "Mapped" = local)
    if (!global && isTRUE(item$inherit.aes)) layers[["Inherited"]] <- plot$mapping
    layers[["Stat default"]] <- item$stat$default_aes
    layers[["Geom default"]] <- item$geom$default_aes
    for (source in names(layers)) {
      if (!key %in% names(layers[[source]])) next
      value <- layers[[source]][[key]]
      out$source[i] <- if (is.null(value) && source == "Mapped") "Suppressed" else source
      out$value[i] <- describe_aes(value)
      break
    }
    matches <- vapply(strsplit(as.character(required), "|", fixed = TRUE), function(x) key %in% x, logical(1))
    out$required_group[i] <- paste(unique(required[matches]), collapse = ", ")
  }
  out
}

edit_plot_aes <- function(plot, index, key, mode, text) {
  if (!key %in% list_aesthetics(plot, index)$aesthetic) stop("Select an aesthetic for this scope.")
  item <- if (index > 0L) plot$layers[[index]]
  mapping <- if (index == 0L) plot$mapping else item$mapping
  if (is.null(mapping)) mapping <- ggplot2::aes()
  fixed <- item$aes_params
  mapping <- mapping[setdiff(names(mapping), key)]
  fixed[[key]] <- NULL
  if (mode %in% c("column", "expression", "fixed")) {
    if (!nzchar(trimws(text))) stop("Enter a value or select a column.")
    expr <- if (mode == "column") rlang::sym(text) else rlang::parse_expr(text)
    if (mode == "fixed") {
      if (is.null(item) || !key %in% item$geom$aesthetics())
        stop("Use a mapping for this scope or stat-only aesthetic.")
      value <- rlang::eval_tidy(expr, env = plot$plot_env)
      if (is.null(value)) stop("Use Remove to clear an aesthetic.")
      if (key == "alpha" && is.numeric(value) && any(value < 0 | value > 1, na.rm = TRUE))
        stop("Alpha must be between 0 and 1.")
      fixed[[key]] <- value
    } else {
      mapping[[key]] <- rlang::new_quosure(expr, plot$plot_env)
    }
  } else if (mode == "remove") {
    if (!is.null(item) && isTRUE(item$inherit.aes) && key %in% names(plot$mapping))
      mapping[key] <- ggplot2::aes(placeholder = NULL)
  } else if (mode != "reset") stop("Unknown edit mode.")
  if (index == 0L) {
    plot$mapping <- mapping
  } else {
    # Layer objects use reference semantics. Detach the edited instance from the input plot.
    copy <- rlang::env_clone(item)
    attributes(copy) <- attributes(item)
    copy$mapping <- mapping
    copy$aes_params <- fixed
    plot$layers[[index]] <- copy
  }
  plot
}
