# Match a visible exterior/interior axis to the nearest panel on its inward side.
axis_panel <- function(axis, panels, side) {
  distance <- switch(side, b = axis$t - panels$t, t = panels$t - axis$t,
                     l = panels$l - axis$l, r = axis$l - panels$l)
  aligned <- if (side %in% c("b", "t")) panels$l == axis$l else panels$t == axis$t
  eligible <- which(aligned & distance > 0)
  if (!length(eligible)) return(integer())
  eligible[which.min(distance[eligible])]
}

tick_key <- function(facets, side, value) {
  as.character(jsonlite::toJSON(list(facets = facets, side = side, value = value),
                                auto_unbox = TRUE, digits = NA))
}

apply_ticks <- function(layout, build, edits, capture = NULL) {
  seen <- character()
  coord <- build$layout$coord
  facet <- build$layout$facet
  supported <- class(coord)[1] %in% c("CoordCartesian", "CoordFixed") &&
    class(facet)[1] %in% c("FacetNull", "FacetWrap", "FacetGrid")
  panels <- subset(layout$layout, grepl("^panel($|-)", name))
  info <- build$layout$layout
  facet_vars <- intersect(unique(c(names(facet$params$facets), names(facet$params$rows),
                                   names(facet$params$cols))), names(info))
  axes <- which(grepl("^axis-[tblr]($|-)", layout$layout$name))
  for (i in if (supported) axes else integer()) {
    axis <- layout$layout[i, ]
    side <- substr(axis$name, 6, 6)
    j <- axis_panel(axis, panels, side)
    if (!length(j)) next
    row <- match(panels$t[j], sort(unique(panels$t)))
    col <- match(panels$l[j], sort(unique(panels$l)))
    panel <- info[info$ROW == row & info$COL == col, , drop = FALSE]
    if (nrow(panel) != 1L) next
    horizontal <- side %in% c("b", "t")
    facet_target <- if (!is.null(capture)) facet_tick_target(build, which(info$PANEL == panel$PANEL),
                                                           if (horizontal) "x" else "y")
    view <- build$layout$panel_params[[as.integer(panel$PANEL)]][[if (horizontal) "x" else "y"]]
    if (!identical(view$position, switch(side, b="bottom", t="top", l="left", r="right"))) next
    breaks <- view$get_breaks()
    values <- if (view$scale$is_discrete()) as.character(breaks) else
      view$scale$get_transformation()$inverse(breaks)
    positions <- as.numeric(view$break_positions())
    labels <- as.character(view$get_labels())
    facets <- lapply(panel[facet_vars], as.character)
    outer <- layout$grobs[[i]]
    inner <- outer$children$axis
    if (is.null(inner)) next
    for (k in seq_along(inner$grobs)) {
      title <- inner$grobs[[k]]
      if (!inherits(title, "titleGrob") || length(title$children) != 1L) next
      node <- title$children[[1]]
      if (!inherits(node, "text") || !is.character(node$label) || isTRUE(node$check.overlap)) next
      pos <- if (horizontal) node$x else node$y
      if (!all(grid::unitType(pos) %in% c("native", "npc"))) next
      keys <- rep("", length(node$label))
      for (n in seq_along(keys)) {
        idx <- which(abs(positions - as.numeric(pos[n])) < 1e-7 & labels == node$label[n])
        if (length(idx) != 1L || !nzchar(node$label[n])) next
        keys[n] <- tick_key(facets, side, values[[idx]])
        if (!is.null(capture)) {
          capture$names[[keys[n]]] <- paste(view$position, "tick", node$label[n], paste(facets, collapse = ", "))
          capture$facet_targets[[keys[n]]] <- facet_target
        }
      }
      valid <- which(nzchar(keys))
      if (!length(valid)) next
      seen <- c(seen, keys[valid])
      for (n in valid) {
        offset <- edits[[keys[n]]]
        if (is.null(offset)) next
        node$x[n] <- node$x[n] + grid::unit(offset[1], "mm")
        node$y[n] <- node$y[n] + grid::unit(offset[2], "mm")
      }
      if (!is.null(capture)) {
        node$keys <- keys
        node$capture <- capture
        class(node) <- c("figure_tick_text", class(node))
      }
      title$children[[1]] <- node
      inner$grobs[[k]] <- title
    }
    outer$children$axis <- inner
    layout$grobs[[i]] <- outer
  }
  missing <- setdiff(names(edits), seen)
  if (length(missing)) warning("Unresolved tick edits: ", paste(missing, collapse = ", "), call. = FALSE)
  layout
}

# Measure each label on the active preview device inside its axis viewport.
drawDetails.figure_tick_text <- function(x, recording) {
  NextMethod()
  xs <- rep(x$x, length.out = length(x$keys))
  ys <- rep(x$y, length.out = length(x$keys))
  for (n in which(nzchar(x$keys))) {
    one <- grid::textGrob(x$label[n], xs[n], ys[n], just = x$just,
                          hjust = rep_len(x$hjust, length(x$label))[n],
                          vjust = rep_len(x$vjust, length(x$label))[n], rot = rep_len(x$rot, length(x$keys))[n])
    lo <- grid::deviceLoc(grid::grobX(one, 180), grid::grobY(one, 270), valueOnly = TRUE)
    hi <- grid::deviceLoc(grid::grobX(one, 0), grid::grobY(one, 90), valueOnly = TRUE)
    x$capture$hits[[x$keys[n]]] <- c(left = lo$x, bottom = lo$y, right = hi$x, top = hi$y)
  }
}
