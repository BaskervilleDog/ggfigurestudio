# Register structural figure elements independently of the semantic tick index.
apply_elements <- function(layout, build, edits, capture, connect_labels = FALSE, previous = NULL) {
  seen <- character()
  register <- function(key, name) {
    seen <<- c(seen, key)
    if (!is.null(capture)) capture$names[[key]] <- name
    edits[[key]]
  }
  panels <- layout$layout[grepl("^panel($|-)", layout$layout$name), ]
  panel_id <- function(cell) {
    info <- build$layout$layout
    row <- match(cell$t, sort(unique(panels$t)))
    col <- match(cell$l, sort(unique(panels$l)))
    panel <- info[info$ROW == row & info$COL == col, , drop = FALSE]
    facet <- build$layout$facet$params
    vars <- intersect(unique(c(names(facet$facets), names(facet$rows), names(facet$cols))), names(info))
    if (nrow(panel) != 1L || !length(vars)) return(cell$name)
    as.character(jsonlite::toJSON(lapply(panel[vars], as.character), auto_unbox = TRUE))
  }
  facet <- build$layout$facet
  original_strips <- if (supported_facet(facet)) {
    base <- if (is.null(facet$figureedit_base)) facet else facet$figureedit_base
    base$format_strip_labels(build$layout$layout, build$layout$facet_params)
  }
  named_roles <- c(title = "Plot title", subtitle = "Subtitle", caption = "Caption",
                   tag = "Plot tag", "xlab-b" = "Bottom axis title", "xlab-t" = "Top axis title",
                   "ylab-l" = "Left axis title", "ylab-r" = "Right axis title")
  link <- function(key, probe, index) {
    if (!is.null(probe) && index <= nrow(probe$coords))
      capture$links[[key]] <- paste("mark", probe$layer, probe$coords$group[index], sep = "/")
  }
  visit <- function(g, path, role, by_label = TRUE, probe = NULL, label_index = 1L,
                    guide = NULL, guide_role = NULL, axis_title = NULL,
                    strip = NULL, strip_column = NULL) {
    if (inherits(g, "figure_shape_annotation")) return(g)
    if (inherits(g, "figure_mark_wrapper")) {
      mark <- g$children[[2]]
      if (!mark$kind %in% c("Text", "Boxed label")) mark <- NULL
      g$children[[1]] <- visit(g$children[[1]], path, role, by_label, mark)
      return(g)
    }
    if (inherits(g, "polyline") && !is.null(capture) &&
        grepl("^panel.grid.(major|minor).[xy]", g$name)) {
      setting <- sub("[.][.].*$", "", g$name)
      key <- paste0("grid/", setting)
      capture$marks[[key]] <- list(title = paste(tools::toTitleCase(sub("panel.grid.", "", setting, fixed = TRUE)), "grid lines"),
        colour = g$gp$col, linewidth = g$gp$lwd * 25.4 / 72.27, linetype = g$gp$lty)
      g$key <- key
      g$capture <- capture
      class(g) <- c("figure_grid_hit", class(g))
    }
    if (inherits(g, "figure_tick_text")) return(g)
    if (inherits(g, "text") && !isTRUE(g$check.overlap)) {
      labels <- as.character(g$label)
      keys <- rep("", length(labels))
      connectors <- list()
      for (n in which(!is.na(labels) & nzchar(labels))) {
        legend_target <- legend_text_target(guide, guide_role, labels[n])
        original <- if (is.null(legend_target)) labels[n] else legend_target$original
        if (length(strip_column) && strip_column <= ncol(strip$original)) {
          original <- as.character(strip$original[[strip_column]][1L])
        }
        suffix <- if (by_label) paste0("/", utils::URLencode(original, reserved = TRUE),
                                       "/", sum(labels[seq_len(n)] == labels[n], na.rm = TRUE)) else ""
        keys[n] <- paste0(path, suffix)
        link(keys[n], probe, n)
        if (!is.null(capture)) {
          if (!is.null(axis_title)) capture$text_targets[[keys[n]]] <-
            list(type = "axis", target = axis_title, displayed = labels[n])
          if (length(strip_column) && strip_column <= ncol(strip$original)) {
            capture$text_targets[[keys[n]]] <- list(type = "facet",
              target = list(part = strip$part, id = strip$id, column = strip_column),
              displayed = labels[n])
            capture$facet_targets[[keys[n]]] <- strip$ticks
          }
          if (!is.null(legend_target)) capture$text_targets[[keys[n]]] <-
            list(type = "legend", target = legend_target, displayed = labels[n])
        }
        offset <- register(keys[n], paste0(role, " - ", labels[n]))
        if (!is.null(offset)) {
          g$x <- rep(g$x, length.out = length(labels))
          g$y <- rep(g$y, length.out = length(labels))
          original_x <- g$x[n]
          original_y <- g$y[n]
          g$x[n] <- g$x[n] + grid::unit(offset[1], "mm")
          g$y[n] <- g$y[n] + grid::unit(offset[2], "mm")
          if (connect_labels && role == "Panel label") {
            gp <- g$gp
            gp[] <- lapply(gp, function(value) rep_len(value, length(labels))[n])
            target <- grid::textGrob(g$label[n], g$x[n], g$y[n], just = g$just,
              hjust = rep_len(g$hjust, length(labels))[n], vjust = rep_len(g$vjust, length(labels))[n],
              rot = rep_len(g$rot, length(labels))[n], gp = gp)
            connectors[[length(connectors) + 1L]] <- label_connector(original_x, original_y, offset, target,
                                                                      previous[[keys[n]]])
          }
        }
      }
      if (!is.null(capture)) {
        g$keys <- keys
        g$capture <- capture
        class(g) <- c("figure_tick_text", class(g))
      }
      if (length(connectors)) g <- grid::gTree(children = do.call(grid::gList, c(connectors, list(g))))
    } else if (inherits(g, "gtable")) {
      for (i in seq_along(g$grobs)) {
        if (role == "Legend text" && startsWith(g$layout$name[i], "key-")) next
        child_guide <- guide
        if (role == "Legend text" && g$layout$name[i] == "guides")
          child_guide <- match_legend_table(g$grobs[[i]], build$plot$guides$params)
        label_cells <- which(startsWith(g$layout$name, "label"))
        if (!is.null(guide) && length(label_cells) == length(guide$key$.label) && i %in% label_cells)
          child_guide$figureedit_label_row <- match(i, label_cells)
        g$grobs[[i]] <- visit(g$grobs[[i]], paste(path, g$layout$name[i], i, sep = "/"),
                             role, by_label, guide = child_guide, guide_role = g$layout$name[i],
                             strip = strip, strip_column = if (!is.null(strip)) i)
        if (grepl("^strip", g$layout$name[i])) g$layout$clip[i] <- "off"
      }
    } else if (inherits(g, "gTree")) {
      # A geom_label box and its text share a positioning viewport.
      if ("box" %in% names(g$children) && inherits(g$children$box, "roundrect") &&
          inherits(g$vp, "viewport")) {
        title <- Filter(function(x) inherits(x, "titleGrob"), as.list(g$children))
        if (length(title) == 1L && length(title[[1]]$children) == 1L) {
          label <- as.character(title[[1]]$children[[1]]$label)
          key <- paste(path, utils::URLencode(label[1], reserved = TRUE), sep = "/")
          link(key, probe, label_index)
          offset <- register(key, paste("Label -", label[1]))
          original <- g$vp
          if (!is.null(offset)) {
            g$vp$x <- g$vp$x + grid::unit(offset[1], "mm")
            g$vp$y <- g$vp$y + grid::unit(offset[2], "mm")
          }
          if (!is.null(capture)) {
            idx <- which(vapply(g$children, inherits, logical(1), "titleGrob"))
            node <- g$children[[idx]]$children[[1]]
            node$keys <- key
            node$capture <- capture
            class(node) <- c("figure_tick_text", class(node))
            g$children[[idx]]$children[[1]] <- node
          }
          if (connect_labels && role == "Panel label" && length(offset)) {
            box <- g$children$box
            centre <- grid::unit(0.5, "npc")
            target <- grid::rectGrob(g$vp$x + box$x - centre, g$vp$y + box$y - centre,
                                     box$width, box$height, just = box$just, gp = box$gp)
            connector <- label_connector(original$x, original$y, offset, target, previous[[key]])
            if (!is.null(connector)) g <- grid::grobTree(connector, g)
          }
          return(g)
        }
      }
      for (i in seq_along(g$children)) {
        g$children[[i]] <- visit(g$children[[i]], paste(path, i, sep = "/"), role, by_label, probe, i,
                                 guide, guide_role, axis_title, strip, strip_column)
      }
    }
    g
  }
  for (i in seq_along(layout$grobs)) {
    cell <- layout$layout[i, ]
    name <- cell$name
    g <- layout$grobs[[i]]
    path <- paste0("element/", name)
    axis_side <- if (grepl("^(axis-|[xy]lab-)[btlr]", name)) substr(name, 6, 6)
    axis_group <- if (length(axis_side)) paste0("element/axis-", axis_side)
    if (name %in% names(named_roles)) {
      target <- if (length(axis_side)) axis_title_target(build, name)
      g <- visit(g, path, named_roles[[name]], FALSE, axis_title = target)
    } else if (grepl("^panel($|-)", name)) {
      g <- visit(g, paste0("element/panel/", panel_id(cell)), "Panel label")
      if (!is.null(capture) && (supported_facet(facet) || inherits(facet, "FacetNull"))) {
        panel <- match(name, panels$name)
        row <- match(panels$t[panel], sort(unique(panels$t)))
        col <- match(panels$l[panel], sort(unique(panels$l)))
        info <- build$layout$layout
        ticks <- facet_tick_target(build, which(info$ROW == row & info$COL == col))
        if (!is.null(ticks)) {
          key <- paste0("panel-ticks/", ticks$id)
          capture$names[[key]] <- if (length(ticks$label)) paste("Facet panel -", ticks$label) else "Plot panel"
          capture$facet_targets[[key]] <- ticks
          probe <- grid::grob(key = key, capture = capture, cl = "figure_legend_probe")
          g <- grid::gTree(children = grid::gList(g, probe))
        }
      }
    } else if (grepl("^axis-", name)) {
      g <- visit(g, path, "Axis label")
      if (!is.null(capture) && !inherits(g, "zeroGrob")) {
        side <- substr(name, 6, 6)
        panel <- facet_cell_panel(cell, panels, build$layout$layout, side)
        capture$facet_targets[[path]] <- facet_tick_target(build, panel,
          if (side %in% c("b", "t")) "x" else "y")
        capture$names[[path]] <- paste(tools::toTitleCase(c(b = "bottom", t = "top", l = "left", r = "right")[[substr(name, 6, 6)]]), "axis")
        probe <- grid::grob(key = path, capture = capture, cl = "figure_legend_probe")
        g <- grid::gTree(children = grid::gList(g, probe))
      }
    } else if (grepl("^strip-", name)) {
      target <- if (supported_facet(facet)) facet_strip_target(build, cell, panels, original_strips)
      g <- visit(g, path, "Facet label", strip = target)
      layout$layout$clip[i] <- "off"
    } else if (grepl("^guide-box", name) && inherits(g, "gtable") && inherits(g$vp, "viewport")) {
      offset <- register(path, "Legend (whole group)")
      if (!is.null(offset)) {
        g$vp$x <- g$vp$x + grid::unit(offset[1], "mm")
        g$vp$y <- g$vp$y + grid::unit(offset[2], "mm")
      }
      g <- visit(g, path, "Legend text")
      if (!is.null(capture)) {
        idx <- which(g$layout$name == "guides")
        for (j in idx) {
          probe <- grid::grob(key = path, capture = capture, cl = "figure_legend_probe")
          g$grobs[[j]] <- grid::gTree(children = grid::gList(g$grobs[[j]], probe))
        }
      }
      layout$layout$clip[i] <- "off"
    }
    if (length(axis_group) && !inherits(g, "zeroGrob")) {
      seen <- c(seen, axis_group)
      offset <- edits[[axis_group]]
      if (!is.null(offset)) {
        # A translated viewport keeps the gtable cell size and its original layout.
        vp <- grid::viewport(x = grid::unit(0.5, "npc") + grid::unit(offset[1], "mm"),
                             y = grid::unit(0.5, "npc") + grid::unit(offset[2], "mm"),
                             clip = "off")
        g <- grid::gTree(children = grid::gList(g), vp = vp)
        layout$layout$clip[i] <- "off"
      }
    }
    layout$grobs[[i]] <- g
  }
  missing <- setdiff(names(edits), seen)
  if (length(missing)) warning("Unresolved figure edits: ", paste(missing, collapse = ", "), call. = FALSE)
  layout
}

drawDetails.figure_legend_probe <- function(x, recording) {
  lo <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE)
  hi <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"), valueOnly = TRUE)
  box <- c(lo$x, lo$y, hi$x, hi$y)
  old <- x$capture$hits[[x$key]]
  if (length(old)) box <- c(pmin(old[1:2], box[1:2]), pmax(old[3:4], box[3:4]))
  x$capture$hits[[x$key]] <- box
}

