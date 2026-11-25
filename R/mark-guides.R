tag_guide_layer <- function(layer, index) {
  force(index)
  geom <- layer$geom
  copy <- rlang::env_clone(layer)
  attributes(copy) <- attributes(layer)
  copy$geom <- ggplot2::ggproto(NULL, geom,
    use_defaults = function(data, ...) {
      key <- geom$use_defaults(data, ...)
      if (".id" %in% names(data)) key$.figureedit_layer <- rep(index, nrow(key))
      key
    })
  copy
}

# A legend key may represent multiple groups, e.g. colour keys for lines also
# grouped by linetype. Only apply styles shared by every represented group.
style_legend_decor <- function(decor, key, aesthetics, layer, data, scales) {
  if (!nrow(data) || !length(data$group)) return(decor)
  styles <- layer$geom$figure_group_styles
  fields <- unique(unlist(lapply(styles, names), use.names = FALSE))
  matching <- intersect(aesthetics, intersect(names(key), names(data)))
  mapped <- lapply(matching, function(aes) {
    scale <- scales$get_scales(aes)
    if (is.null(scale)) data[[aes]] else scale$map(data[[aes]])
  })
  drawn <- decor$data$.draw
  if (is.null(drawn)) drawn <- rep(TRUE, nrow(decor$data))
  for (row in which(drawn %in% TRUE)) {
    key_row <- decor$data$.id[row]
    groups <- as.character(unique(data$group))
    if (length(matching)) {
      keep <- rep(TRUE, nrow(data))
      for (i in seq_along(matching)) {
        aes <- matching[i]
        value <- key[[aes]][key_row]
        # Use raw discrete break values where they identify the same mapped key.
        scale <- scales$get_scales(aes)
        raw <- key$.value[key_row]
        if (!is.null(scale) && scale$is_discrete() && length(raw) &&
            isTRUE(all.equal(unname(scale$map(raw)), unname(value))) &&
            any(data[[aes]] %in% raw)) {
          keep <- keep & data[[aes]] %in% raw
        } else {
          keep <- keep & mapped[[i]] %in% value
        }
      }
      groups <- as.character(unique(data$group[keep]))
    }
    if (!length(groups)) next
    for (field in fields) {
      values <- lapply(groups, function(group) styles[[group]][[field]])
      value <- values[[1]]
      if (length(value) && all(vapply(values, identical, logical(1), y = value))) {
        decor$data[[field]] <- style_column(decor$data[[field]], row, value, field)
      }
    }
  }
  decor
}

group_style_guides <- function(guides) {
  if (isTRUE(guides$figureedit_group_styles)) return(guides)
  ggplot2::ggproto(NULL, guides, figureedit_group_styles = TRUE,
    build = function(self, scales, layers, labels, layer_data, theme = NULL) {
      edited <- which(vapply(layers, function(layer) length(layer$geom$figure_group_styles) > 0L,
                             logical(1)))
      tagged <- layers
      for (i in edited) tagged[[i]] <- tag_guide_layer(layers[[i]], i)
      result <- ggplot2::ggproto_parent(guides, self)$build(scales, tagged, labels, layer_data, theme)
      params <- result$params
      for (j in seq_along(params)) {
        guide <- params[[j]]
        if (inherits(result$guides[[j]], "GuideLegend")) {
          for (k in seq_along(guide$decor)) {
            decor <- guide$decor[[k]]
            i <- decor$data$.figureedit_layer[1]
            if (!length(i) || is.na(i) || !i %in% edited) next
            decor$data$.figureedit_layer <- NULL
            guide$decor[[k]] <- style_legend_decor(decor, guide$key, guide$aesthetic,
                                                  layers[[i]], layer_data[[i]], scales)
          }
        }
        id <- guide_text_id(guide)
        if (nzchar(id)) guide <- apply_guide_text(guide, self$figureedit_text[[id]])
        params[[j]] <- guide
      }
      result$params <- params
      result
    })
}

with_group_style_guides <- function(plot) {
  edited <- vapply(plot$layers, function(layer) length(layer$geom$figure_group_styles) > 0L,
                   logical(1))
  if (any(edited)) plot$guides <- group_style_guides(plot$guides)
  plot
}