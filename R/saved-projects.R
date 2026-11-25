# Older RDS files can contain ggproto methods from the former package namespace.
# Rebuild those methods from the saved style and annotation settings.
restore_saved_plot <- function(plot) {
  legacy_method <- function(object, method) {
    fun <- get0(method, envir = object, inherits = FALSE)
    is.function(fun) && !identical(topenv(environment(fun)), asNamespace("ggfigurestudio"))
  }
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    geom <- layer$geom
    if (inherits(geom, "GeomFigureShape") && legacy_method(geom, "draw_panel")) {
      replacement <- GeomFigureShape
    } else if (!is.null(geom$figure_base_geom) && legacy_method(geom, "draw_layer")) {
      replacement <- mark_geom(geom, geom$figure_group_styles,
        tile_styles = geom$figure_tile_styles, tile_layer_style = geom$figure_tile_layer_style)
    } else next
    copy <- rlang::env_clone(layer)
    attributes(copy) <- attributes(layer)
    copy$geom <- replacement
    plot$layers[[i]] <- copy
  }
  facet <- plot$facet
  if (!is.null(facet$figureedit_base) && legacy_method(facet, "attach_axes"))
    plot$facet <- new_facet_settings(facet, facet$figureedit_titles, facet$figureedit_breaks)
  if (isTRUE(plot$layout$figureedit_facet_ticks) && legacy_method(plot$layout, "setup_panel_guides"))
    plot$layout <- facet_tick_layout(plot$layout$super())
  guides <- plot$guides
  if (isTRUE(guides$figureedit_group_styles) && legacy_method(guides, "build")) {
    restored <- group_style_guides(guides$super())
    restored$guides <- guides$guides
    restored$figureedit_text <- guides$figureedit_text
    plot$guides <- restored
  }
  plot
}
