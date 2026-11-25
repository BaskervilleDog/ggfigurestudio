element_context <- function(key) {
  if (startsWith(key, "panel-ticks/")) return(list(heading = "Panel ticks"))
  part <- strsplit(key, "/", fixed = TRUE)[[1]]
  role <- if (length(part) >= 2L && part[1] == "element") part[2] else ""
  sides <- c(b = "bottom", t = "top", l = "left", r = "right")
  side <- if (grepl("^(axis-|[xy]lab-)[btlr]", role)) {
    sub("^(axis-|[xy]lab-)([btlr]).*$", "\\2", role)
  } else if (startsWith(key, "{")) {
    jsonlite::fromJSON(key)$side
  }
  if (length(side) && side %in% names(sides)) {
    axis <- if (side %in% c("b", "t")) "x" else "y"
    suffix <- paste(axis, sides[[side]], sep = ".")
    heading <- paste(toupper(axis), "axis")
    if (grepl("^[xy]lab-", role)) {
      return(list(heading = paste(heading, "title", paste0("(", sides[[side]], ")")),
                  text = paste0("axis.title.", suffix), text_name = "Title"))
    }
    return(list(heading = paste(heading, paste0("(", sides[[side]], ")")),
                text_name = "Tick label",
                text = paste0("axis.text.", suffix), title = paste0("axis.title.", suffix),
                line = paste0("axis.line.", suffix), ticks = paste0("axis.ticks.", suffix)))
  }
  if (role %in% c("title", "subtitle", "caption", "tag")) {
    return(list(heading = switch(role, title = "Plot title", subtitle = "Subtitle",
                               caption = "Caption", tag = "Plot tag"),
                text = paste0("plot.", role), label = role))
  }
  if (startsWith(role, "guide-box")) {
    text <- if (grepl("/title/", key, fixed = TRUE)) "legend.title" else "legend.text"
    return(list(heading = "Legend", text = text, legend = TRUE))
  }
  if (startsWith(role, "strip-")) {
    side <- substr(role, 7, 7)
    axis <- if (side %in% c("t", "b")) "x" else "y"
    return(list(heading = "Facet labels", text = paste("strip.text", axis, sides[[side]], sep = ".")))
  }
  list(heading = "Panel label")
}

bind_element_inspector <- function(input, output, session, edited_plot, current, preview, title_override, palettes,
                                   delete_layer, offsets) {
  status <- shiny::reactiveVal("")
  selection <- shiny::reactiveVal(NULL)
  selected_mark <- shiny::reactiveVal(NULL)
  tile_scope <- shiny::reactiveVal("cell")
  shiny::observeEvent(input$tile_scope, {
    if (input$tile_scope %in% c("cell", "layer")) tile_scope(input$tile_scope)
  })
  bind_mark_inspector(input, edited_plot, current, preview, selected_mark, status, palettes, tile_scope,
                      delete_layer, offsets)
  shiny::observeEvent(input$inspect_element, {
    key <- input$inspect_element
    if (!nzchar(key)) {
      selected_mark(NULL)
      selection(NULL)
      return()
    }
    view <- preview()
    shiny::req(key %in% c(names(view$marks), view$keys, view$axis_keys))
    if (!identical(input$editor_tab, "selection"))
      shiny::updateTabsetPanel(session, "editor_tab", selected = "selection")
    if (key %in% names(view$marks)) {
      selected_mark(key)
      status("")
      return()
    }
    selected_mark(NULL)
    context <- element_context(key)
    context$key <- key
    selection(context)
    status("")
  })
  settings <- shiny::reactive({
    context <- shiny::req(selection())
    plot <- edited_plot()
    theme <- ggplot2::theme_get() + plot$theme
    visible <- function(name) !inherits(ggplot2::calc_element(name, theme), "element_blank")
    element <- if (length(context$text)) ggplot2::calc_element(context$text, theme)
    text <- if (inherits(element, "element_text")) element else
      ggplot2::calc_element("text", theme)
    position <- theme$legend.position
    target <- if (length(context$key)) shiny::isolate(preview()$text_targets[[context$key]])
    list(context = context, size = text$size, angle = text$angle, colour = text$colour,
         target = target,
         facet = if (length(context$key)) shiny::isolate(preview()$facet_targets[[context$key]]),
         label = if (!is.null(target)) target$displayed
         else if (identical(context$label, "title") && !is.null(title_override()))
           title_override() else if (length(context$label)) plot$labels[[context$label]],
         labels = if (length(context$text)) visible(context$text),
         axis_title = if (length(context$title)) visible(context$title),
         ticks = if (length(context$ticks)) visible(context$ticks),
         line = if (length(context$line)) visible(context$line),
         position = if (is.character(position)) position else "inside")
  })
  output$element_options <- shiny::renderUI({
    key <- selected_mark()
    if (length(key)) {
      view <- preview()
      mark <- shiny::req(view$marks[[key]])
      facet_editor <- if (isTRUE(mark$faceted)) {
        choices <- annotation_copy_choices(edited_plot()$layers[[mark$layer]], view$annotation_facets)
        annotation_edit_controls(mark, choices, shiny::isolate(input$annotation_edit_mode),
                                 shiny::isolate(input$annotation_edit_facets))
      }
      return(mark_inspector(mark, key, palettes(), tile_scope(), facet_editor))
    }
    if (is.null(selection())) return(shiny::tagList(
      shiny::h2("Inspect an element"),
      shiny::p(class = "editor-help",
        "Click a title, axis, legend, data mark, or annotation in the preview to see its editing options.")))
    s <- settings()
    context <- s$context
    text_name <- if (is.null(context$text_name)) "Text" else context$text_name
    shiny::tagList(
      shiny::h4(context$heading),
      if (length(context$title)) editor_group("Axis visibility", editor_fields(
          shiny::checkboxInput("show_axis_title", "Show axis title", s$axis_title),
          shiny::checkboxInput("show_tick_labels", "Show tick labels", s$labels),
          shiny::checkboxInput("show_ticks", "Show tick marks", s$ticks),
          shiny::checkboxInput("show_axis_line", "Show axis line", s$line))),
      if (length(context$text)) editor_group("Text appearance",
        if (length(context$label) || !is.null(s$target)) shiny::textInput("element_label", "Text",
          if (is.character(s$label) && length(s$label) == 1L) s$label else ""),
        shiny::tags$div(class = "editor-fields editor-text-measurements",
          shiny::numericInput("element_size", paste(text_name, "size (pt)"), s$size, min = 1),
          shiny::numericInput("element_angle", paste(text_name, "angle (degrees)"), s$angle, min = -360, max = 360)),
        shiny::textInput("element_colour", paste(text_name, "colour"), s$colour)),
      if (isTRUE(context$legend)) editor_group("Legend layout",
        editor_select("element_legend", "Legend position",
          c("right", "left", "top", "bottom", "inside", "none"), selected = s$position)),
      if (length(context$text)) editor_actions(
        shiny::actionButton("apply_element", "Apply display settings", class = "btn-primary")),
      facet_break_inputs(s$facet),
      if (is.null(context$text) && is.null(s$facet)) shiny::p("Drag or use the arrow keys to move this label.")
    )
  })
  output$element_status <- shiny::renderText(status())
  shiny::observeEvent(input$apply_facet_breaks, {
    tryCatch({
      facet <- shiny::req(settings()$facet)
      values <- lapply(names(facet$values), function(axis) {
        parse_facet_breaks(input[[paste0("facet_breaks_", axis)]])
      })
      names(values) <- names(facet$values)
      candidate <- set_facet_breaks(edited_plot(), facet$id, values)
      trial <- current()
      trial$plot <- candidate
      tick_preview(trial)
      edited_plot(candidate)
      status(if (length(facet$label)) "Facet ticks applied." else "Ticks applied.")
    }, error = function(e) status(paste("Could not apply ticks.", conditionMessage(e))))
  })
  shiny::observeEvent(input$apply_element, {
    tryCatch({
      s <- settings()
      context <- s$context
      values <- list(size = input$element_size, angle = input$element_angle,
                     colour = trimws(input$element_colour))
      stopifnot(length(values$size) == 1L, is.finite(values$size), values$size > 0,
                length(values$angle) == 1L, is.finite(values$angle), abs(values$angle) <= 360)
      grDevices::col2rgb(values$colour)
      changes <- values[!vapply(names(values), function(n) identical(s[[n]], values[[n]]), logical(1))]
      theme <- list()
      labels_visible <- is.null(context$title) || isTRUE(input$show_tick_labels)
      if (length(changes) || !identical(labels_visible, s$labels)) {
        theme[[context$text]] <- if (labels_visible) {
          if (isTRUE(s$labels)) do.call(ggplot2::element_text, changes)
          else do.call(ggplot2::element_text, c(values, list(inherit.blank = FALSE)))
        } else ggplot2::element_blank()
      }
      if (length(context$title)) {
        if (!identical(input$show_axis_title, s$axis_title))
          theme[[context$title]] <- if (isTRUE(input$show_axis_title)) ggplot2::element_text(inherit.blank = FALSE) else ggplot2::element_blank()
        if (!identical(input$show_ticks, s$ticks))
          theme[[context$ticks]] <- if (isTRUE(input$show_ticks)) ggplot2::element_line(inherit.blank = FALSE) else ggplot2::element_blank()
        if (!identical(input$show_axis_line, s$line))
          theme[[context$line]] <- if (isTRUE(input$show_axis_line)) ggplot2::element_line(inherit.blank = FALSE) else ggplot2::element_blank()
      }
      if (isTRUE(context$legend) && !identical(input$element_legend, s$position))
        theme$legend.position <- input$element_legend
      candidate <- edited_plot()
      if (length(theme)) candidate <- candidate + do.call(ggplot2::theme, theme)
      label_changed <- (length(context$label) || !is.null(s$target)) && !identical(input$element_label,
        if (is.character(s$label) && length(s$label) == 1L) s$label else "")
      if (label_changed) {
        if (is.null(s$target)) {
          candidate <- candidate + do.call(ggplot2::labs, stats::setNames(list(input$element_label), context$label))
        } else if (s$target$type == "axis") {
          candidate <- set_axis_title(candidate, s$target$target, input$element_label)
        } else if (s$target$type == "facet") {
          candidate <- set_facet_title(candidate, s$target$target, input$element_label)
        } else {
          candidate <- set_legend_text(candidate, s$target$target, input$element_label)
        }
      }
      trial <- current()
      trial$plot <- candidate
      clear_title <- label_changed && identical(context$label, "title")
      if (clear_title) trial$title <- NULL
      tick_preview(trial)
      if (clear_title) title_override(NULL)
      edited_plot(candidate)
      status("Applied.")
    }, error = function(e) status(paste("Could not apply settings.", conditionMessage(e))))
  })
}
