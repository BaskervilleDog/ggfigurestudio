mark_font_control <- function(field, current) {
  family <- field == "family"
  choices <- if (family) c(`Device default` = "[device default]", editor_font_families())
    else c(editor_font_faces, Symbol = "symbol")
  if (!family) current <- unique(mark_fontface_name(current))
  selected <- if (length(current) == 1L && !is.na(current)) as.character(current) else ""
  if (family && identical(current, "")) selected <- "[device default]"
  if (!nzchar(selected)) {
    label <- if (length(current) > 1L) "Varies" else "Choose a value"
    choices <- c(stats::setNames("", label), choices)
  } else if (!selected %in% choices) {
    choices <- c(choices, stats::setNames(selected, selected))
  }
  editor_select(paste0("mark_", field), if (family) "Font family" else "Font style",
                choices = choices, selected = selected, width = "100%")
}

mark_inspector <- function(mark, key, palettes, tile_scope = NULL, facet_editor = NULL) {
  if (identical(mark$kind, "Shape")) {
    annotation <- mark$annotation
    name <- shape_edit_name(annotation)
    return(shiny::tagList(shiny::h4(shape_title(annotation)),
      facet_editor,
      if (!isTRUE(mark$faceted)) shiny::helpText("Centre x and y set the anchor. Dragging and arrow keys offset it."),
      shape_fields("edit_shape_", annotation),
      editor_actions(
        shiny::actionButton("apply_shape_edit", paste("Apply", name, "edit"), class = "btn-primary"),
        shiny::actionButton("delete_shape",
          paste("Delete", name, if (isTRUE(mark$faceted)) "in all facets" else NULL)))))
  }
  if (startsWith(key, "grid/")) {
    return(shiny::tagList(
      shiny::h4(mark$title),
      editor_group("Line appearance",
        editor_fields(shiny::textInput("mark_colour", "Colour", mark$colour),
                      shiny::numericInput("mark_linewidth", "Line width (mm)", mark$linewidth, min = 0)),
        shiny::textInput("mark_linetype", "Line type", mark$linetype)),
      editor_actions(shiny::actionButton("apply_mark", "Apply grid style", class = "btn-primary"))))
  }
  cell <- !is.null(mark$cell)
  all_cells <- cell && identical(tile_scope, "layer")
  scope <- if (all_cells) "layer" else if (cell) {
    switch(mark$kind, Area = "area", `Bar segment` = "segment", "cell")
  } else "group"
  values <- if (all_cells) mark$layer_values else mark$values
  shiny::tagList(
    shiny::h4(mark$title),
    facet_editor,
    if (cell) {
      choices <- switch(mark$kind,
        Area = c("Selected area" = "cell", "All areas in layer" = "layer"),
        `Bar segment` = c("Selected segment" = "cell", "All segments in layer" = "layer"),
        c("Selected cell" = "cell", "All cells in layer" = "layer"))
      editor_group("Scope", shiny::radioButtons("tile_scope", "Apply to", choices, selected = tile_scope))
    },
    if (length(mark$reference)) editor_group("Position",
      shiny::textInput("mark_intercept", "Position on the axis", mark$reference$value),
      shiny::helpText("Arrow keys offset the copy you click.")
    ),
    if (!cell && !isTRUE(mark$faceted))
      shiny::p(class = "editor-help", "Style changes apply to this group across facets."),
    editor_group("Appearance", editor_fields(
      lapply(names(values), function(field) {
      current <- values[[field]]
      if (field %in% c("family", "fontface")) return(mark_font_control(field, current))
      label <- if (cell) switch(field, colour = "Border colour", linewidth = "Border width",
                                linetype = "Border line type", alpha = "Opacity", "Fill")
        else tools::toTitleCase(field)
      if (identical(mark$kind, "Boxplot") && field %in% c("size", "shape"))
        label <- paste("Outlier", field)
      shiny::textInput(paste0("mark_", field), label,
        value = if (length(current) == 1L && !is.na(current)) as.character(current) else "",
        placeholder = if (length(current) > 1L) "Varies" else "Default")
    }))),
    if (identical(mark$kind, "Point"))
      shiny::p(class = "editor-help", "Use point shapes 21 to 25 to show a separate fill colour."),
    editor_actions(shiny::actionButton("apply_mark", paste("Apply", scope, "style"), class = "btn-primary"),
                   shiny::actionButton("reset_mark", paste("Reset", scope, "style"))),
    mark_palette_input(mark, palettes, all_cells))
}

mark_value <- function(field, text) {
  if (field %in% c("colour", "fill")) {
    grDevices::col2rgb(text)
    return(text)
  }
  numeric_value <- suppressWarnings(as.numeric(text))
  if (field %in% c("size", "stroke", "alpha", "linewidth", "angle", "lineheight", "width", "height")) {
    if (!is.finite(numeric_value) || (field != "angle" && numeric_value < 0) ||
        (field == "alpha" && numeric_value > 1))
      stop("Invalid ", field, ". Enter a numeric value", if (field == "alpha") " from 0 to 1", ".")
    return(numeric_value)
  }
  if (field %in% c("shape", "hjust", "vjust", "fontface") && is.finite(numeric_value)) return(numeric_value)
  if (field == "linetype" && text %in% as.character(0:6)) return(numeric_value)
  text
}

bind_mark_inspector <- function(input, edited_plot, current, preview, selected_mark, status, palettes, tile_scope,
                                delete_layer, offsets) {
  save_facet_position <- function(trial, mark, positions, panels) {
    trial <- set_facet_annotation_position(trial, mark, positions, panels)
    keys <- paste0(sub("/facet/.*$", "", mark$move_key), panels)
    rendered <- Filter(function(copy) length(copy$move_key) && !identical(copy$visible, FALSE),
                       tick_preview(trial)$marks)
    if (!all(keys %in% vapply(rendered, function(copy) copy$move_key, character(1))))
      stop("Enter a position inside every targeted facet.")
    offsets(c(trial$tick_edits, trial$element_edits))
    trial$plot
  }
  shiny::observeEvent(input$delete_shape, {
    mark <- preview()$marks[[shiny::req(selected_mark())]]
    shiny::req(identical(mark$kind, "Shape"))
    tryCatch({
      delete_layer(mark$layer)
      status("")
    }, error = function(e) status(paste("Annotation could not be deleted.", conditionMessage(e))))
  })
  shiny::observeEvent(input$apply_shape_edit, {
    key <- shiny::req(selected_mark())
    mark <- preview()$marks[[key]]
    shiny::req(identical(mark$kind, "Shape"))
    tryCatch({
      annotation <- shape_spec(input, "edit_shape_", mark$annotation$type, previous = mark$annotation)
      trial <- current()
      positions <- NULL
      if (isTRUE(mark$faceted)) {
        layer <- trial$plot$layers[[mark$layer]]
        scope <- annotation_edit_scope(mark, layer, preview()$annotation_facets, input)
        axes <- c("x", "y")
        changed <- vapply(axes, function(axis) !identical(annotation[[axis]], mark$annotation[[axis]]), logical(1))
        positions <- annotation[axes[changed]]
        fields <- setdiff(names(annotation), c("type", axes))
        defaults <- c(shape_style_defaults, list(angle = 0))
        defaults$border_width <- round(defaults$border_width, 2)
        displayed <- utils::modifyList(defaults, mark$annotation)
        changed <- vapply(fields, function(field) {
          !isTRUE(all.equal(annotation[[field]], displayed[[field]]))
        }, logical(1))
        styles <- annotation[fields[changed]]
        if (!scope$shared) {
          trial$plot <- set_facet_annotation_style(trial$plot, mark, styles, panels = scope$panels)
        } else if (length(styles)) {
          trial$plot <- set_facet_annotation_style(trial$plot, mark, styles, all_facets = TRUE)
          updated <- trial$plot$layers[[mark$layer]]
          updated$geom_params$annotation[names(styles)] <- styles
        }
      } else {
        trial$plot <- shape_plot(trial$plot, annotation, mark$layer)
      }
      candidate <- if (length(positions)) save_facet_position(trial, mark, positions, scope$panels) else {
        check_shape_preview(trial, mark$layer)
        trial$plot
      }
      edited_plot(candidate)
      status(paste(tools::toTitleCase(shape_edit_name(annotation)), "updated."))
    }, error = function(e) status(paste("Annotation could not be updated.", conditionMessage(e))))
  })
  shiny::observeEvent(input$apply_point_palette, {
    key <- shiny::req(selected_mark())
    mark <- shiny::req(preview()$marks[[key]])
    cell <- !is.null(mark$cell)
    if (cell) shiny::req(identical(tile_scope(), "layer"), nzchar(mark$palette_type))
    targets <- shiny::req(mark_palette_targets(mark))
    choice <- shiny::req(input$point_palette)
    shiny::req(choice %in% palette_choices(targets))
    tryCatch({
      updated <- palettes()
      selected <- viridis_palette(choice, targets)
      updated[names(targets)] <- selected
      trial <- current()
      trial$point_palettes <- updated
      if (cell) {
        style <- trial$plot$layers[[mark$layer]]$geom$figure_tile_layer_style
        if (!is.null(style$fill)) {
          style$fill <- NULL
          trial$plot <- styled_plot(trial$plot, mark$layer, NULL, style, all_cells = TRUE)
        }
      }
      tick_preview(trial)
      edited_plot(trial$plot)
      palettes(updated)
      status("Palette applied.")
    }, error = function(e) status(paste("Could not apply palette.", conditionMessage(e))))
  })
  apply_style <- function(reset) {
    key <- shiny::req(selected_mark())
    mark <- shiny::req(preview()$marks[[key]])
    plot <- edited_plot()
    positions <- NULL
    tryCatch({
      if (startsWith(key, "grid/")) {
        style <- list(colour = mark_value("colour", input$mark_colour),
                      linewidth = mark_value("linewidth", input$mark_linewidth),
                      linetype = mark_value("linetype", input$mark_linetype))
        theme <- stats::setNames(list(do.call(ggplot2::element_line, style)), sub("^grid/", "", key))
        candidate <- plot + do.call(ggplot2::theme, theme)
      } else {
        geom <- plot$layers[[mark$layer]]$geom
        all_cells <- !is.null(mark$cell) && identical(tile_scope(), "layer")
        displayed <- if (all_cells) mark$layer_values else mark$values
        values <- if (all_cells) geom$figure_tile_layer_style else if (is.null(mark$cell))
          geom$figure_group_styles[[mark$group]] else geom$figure_tile_styles[[mark$cell]]
        changes <- list()
        for (field in if (reset) character() else names(displayed)) {
          text <- trimws(input[[paste0("mark_", field)]])
          old <- displayed[[field]]
          if (!length(text) || !nzchar(text)) next
          if (field == "fontface") {
            old <- unique(mark_fontface_name(old))
            text <- mark_fontface_name(text)
          }
          if (field == "family" && identical(text, "[device default]")) text <- ""
          if (!identical(text, as.character(old))) changes[[field]] <- mark_value(field, text)
        }
        faceted_line <- isTRUE(mark$faceted) && length(mark$reference) > 0L
        scope <- if (faceted_line)
          annotation_edit_scope(mark, plot$layers[[mark$layer]], preview()$annotation_facets, input)
        if (faceted_line && !scope$shared) {
          candidate <- set_facet_annotation_style(plot, mark, changes, reset = reset, panels = scope$panels)
        } else {
          if (reset) values <- NULL else values[names(changes)] <- changes
          candidate <- styled_plot(plot, mark$layer, mark$group, values, cell = mark$cell,
                                   all_cells = all_cells, changed_fields = names(changes))
          if (faceted_line) {
            candidate <- set_facet_annotation_style(candidate, mark, changes, all_facets = TRUE, reset = reset)
          }
        }
        if (!reset && length(mark$reference) && !identical(input$mark_intercept, mark$reference$value)) {
          if (isTRUE(mark$faceted)) {
            positions <- stats::setNames(list(input$mark_intercept), mark$reference$axis)
          } else {
            candidate <- move_reference_line(candidate, mark$layer, mark$reference, input$mark_intercept)
          }
        }
      }
      trial <- current()
      trial$plot <- candidate
      if (length(positions)) candidate <- save_facet_position(trial, mark, positions, scope$panels)
      else tick_preview(trial)
      edited_plot(candidate)
      status("Applied.")
    }, error = function(e) status(paste("Could not apply style.", conditionMessage(e))))
  }
  shiny::observeEvent(input$apply_mark, apply_style(FALSE))
  shiny::observeEvent(input$reset_mark, apply_style(TRUE))
}
