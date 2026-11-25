edit_figure <- function(plot = example_figure(), show = interactive()) {
  initial <- if (inherits(plot, c("figure_project", "figure_composition"))) plot
    else if (is.list(plot) && !inherits(plot, "ggplot")) figure_composition(plot)
    else figure_project(plot)
  result <- shiny::runApp(figure_editor(initial), launch.browser = getOption("shiny.launch.browser", interactive()))
  if (isTRUE(show) && inherits(result, c("figure_project", "figure_composition"))) {
    tryCatch({
      drawing <- render_figure(result)
      grid::grid.newpage()
      grid::grid.draw(drawing)
    }, error = function(e) {
      message("The edited project was returned. The graphics device could not display it. ",
              conditionMessage(e))
    })
  }
  invisible(result)
}

figure_editor <- function(initial) {
  composition <- if (inherits(initial, "figure_composition")) initial
  plot_keys <- if (!is.null(composition)) {
    keys <- paste0("composition/", seq_along(composition$plots))
    keys[1L] <- "original"
    keys
  }
  restore <- function(project) {
    project$plot <- restore_saved_plot(project$plot)
    project
  }
  if (is.null(composition)) {
    initial <- restore(initial)
  } else {
    composition$plots <- lapply(composition$plots, restore)
    initial <- composition$plots[[1L]]
  }
  if (is.null(initial$nudge)) initial$nudge <- 0.5
  ui <- editor_ui(initial, composition)
  server <- function(input, output, session) {
    first_title <- if (!is.null(composition)) names(composition$plots)[1L]
    document <- shiny::reactiveVal(list(key = "original", title = first_title, project = initial))
    saved <- list()
    layout <- shiny::reactiveVal(composition)
    if (!is.null(composition)) {
      for (i in seq_along(plot_keys)[-1L]) {
        project <- composition$plots[[i]]
        if (is.null(project$nudge)) project$nudge <- 0.5
        saved[[plot_keys[i]]] <- list(project = project, history = list(),
          document = list(key = plot_keys[i], title = names(composition$plots)[i], project = project))
      }
    }
    edited_plot <- shiny::reactiveVal(with_group_style_guides(identify_annotation_layers(initial$plot)))
    grid_visibility <- shiny::reactiveVal(initial$grid_visibility)
    palettes <- shiny::reactiveVal(project_palettes(initial))
    title_override <- shiny::reactiveVal(initial$title)
    connect_labels <- shiny::reactiveVal(isTRUE(initial$connect_labels))
    shiny::observeEvent(input$connect_labels, connect_labels(isTRUE(input$connect_labels)))
    anchor_mode <- shiny::reactiveVal(label_anchor_mode(initial))
    shiny::observeEvent(input$label_anchor_mode, {
      shiny::req(input$label_anchor_mode %in% label_anchor_choices)
      anchor_mode(input$label_anchor_mode)
    })
    previous_offsets <- shiny::reactiveVal(initial$previous_label_offsets)
    plot_dimensions <- shiny::reactiveVal(plot_size(initial))
    size_status <- shiny::reactiveVal("")
    output$plot_size_status <- shiny::renderText(size_status())
    offsets <- shiny::reactiveVal(c(initial$tick_edits, initial$element_edits))
    history_status <- shiny::reactiveVal("")
    output$history_status <- shiny::renderText(history_status())
    history <- list()
    current <- shiny::reactive({
      project <- document()$project
      if (document()$key %in% plot_keys) {
        cells <- composition_cells(layout())
        project$width <- cells$width
        project$height <- cells$height
      }
      project$plot <- edited_plot()
      project$grid_visibility <- grid_visibility()
      edits <- offsets()
      element <- startsWith(as.character(names(edits)), "element/")
      project$tick_edits <- edits[!element]
      project$element_edits <- edits[element]
      project$title <- title_override()
      project$connect_labels <- connect_labels()
      project$label_anchor_mode <- anchor_mode()
      project$previous_label_offsets <- previous_offsets()
      project$plot_size <- plot_dimensions()
      project$point_palettes <- palettes()
      project
    })
    preview <- shiny::reactive(tick_preview(current()))
    shiny::observeEvent(input$apply_plot_size, {
      dimensions <- c(width = input$plot_width_percent, height = input$plot_height_percent)
      if (length(dimensions) != 2L || !is.numeric(dimensions) ||
          any(!is.finite(dimensions) | dimensions < 1 | dimensions > 100)) {
        size_status("Enter a width and height between 1% and 100%.")
        return()
      }
      tryCatch({
        trial <- current()
        trial$plot_size <- dimensions
        tick_preview(trial)
        plot_dimensions(dimensions)
        size_status("Plot size applied.")
      }, error = function(e) size_status(paste("Could not resize the plot.", conditionMessage(e))))
    })
    clear_selection <- function() session$sendCustomMessage("clear-figure-selection", TRUE)
    output$preview_figure <- shiny::renderText({
      active <- document()
      project <- if (isTRUE(input$show_composition) && !is.null(layout())) layout() else current()
      size <- paste(project$width, "\u00d7", project$height, "mm")
      if (length(active$title)) paste(active$title, size, sep = " \u00b7 ") else size
    })
    active_project <- function() {
      project <- current()
      nudge <- input$nudge
      if (length(nudge) && is.finite(nudge) && nudge > 0) project$nudge <- nudge
      project$annotation_all_facets <- !identical(input$annotation_all_facets, FALSE)
      project
    }
    switch_figure <- function(key, title = NULL, create = NULL) {
      active <- document()
      if (identical(key, active$key)) return(invisible(NULL))
      old <- active_project()
      next_figure <- saved[[key]]
      if (is.null(next_figure)) {
        canvas <- if (is.null(composition)) old else layout()
        project <- figure_project(create(), width = canvas$width, height = canvas$height)
        project$nudge <- old$nudge
        project$annotation_all_facets <- old$annotation_all_facets
        tick_preview(project)
        next_figure <- list(document = list(key = key, title = title, project = project),
                            project = project, history = list())
      }
      saved[[active$key]] <<- list(document = active, project = old, history = history)
      project <- next_figure$project
      next_figure$document$project$nudge <- project$nudge
      document(next_figure$document)
      edited_plot(with_group_style_guides(identify_annotation_layers(project$plot)))
      grid_visibility(project$grid_visibility)
      palettes(project_palettes(project))
      title_override(project$title)
      connect_labels(isTRUE(project$connect_labels))
      shiny::updateCheckboxInput(session, "connect_labels", value = connect_labels())
      anchor_mode(label_anchor_mode(project))
      shiny::updateSelectizeInput(session, "label_anchor_mode", selected = anchor_mode())
      previous_offsets(project$previous_label_offsets)
      plot_dimensions(plot_size(project))
      size_status("")
      shiny::updateNumericInput(session, "plot_width_percent", value = plot_dimensions()[["width"]])
      shiny::updateNumericInput(session, "plot_height_percent", value = plot_dimensions()[["height"]])
      offsets(c(project$tick_edits, project$element_edits))
      history <<- next_figure$history
      history_status("")
      shiny::updateNumericInput(session, "nudge", value = project$nudge)
      shiny::updateCheckboxInput(session, "annotation_all_facets",
                                 value = !identical(project$annotation_all_facets, FALSE))
      if (!is.null(composition)) shiny::updateCheckboxInput(session, "show_composition", value = FALSE)
      clear_selection()
    }
    if (!is.null(composition)) {
      composition_status <- shiny::reactiveVal("")
      output$composition_status <- shiny::renderText(composition_status())
      combined_project <- function(project = layout()) {
        key <- document()$key
        project$plots <- lapply(plot_keys, function(i) {
          if (identical(i, key)) active_project() else saved[[i]]$project
        })
        names(project$plots) <- names(composition$plots)
        project
      }
      shiny::observeEvent(input$edit_composition_plot, {
        plot <- match(input$composition_plot, as.character(seq_along(plot_keys)))
        shiny::req(!is.na(plot))
        switch_figure(plot_keys[plot])
        shiny::updateCheckboxInput(session, "show_composition", value = FALSE)
      })
      shiny::observeEvent(input$apply_composition_layout, {
        tryCatch({
          candidate <- layout()
          candidate$ncol <- input$composition_columns
          candidate$gap <- input$composition_gap
          composition_preview(combined_project(candidate))
          layout(candidate)
          composition_status("Layout applied.")
        }, error = function(e) composition_status(conditionMessage(e)))
      })
      output$combined_preview <- shiny::renderUI({
        shiny::req(input$show_composition)
        composition_preview(combined_project())
      })
    }
    delete_layer <- function(index) {
      trial <- current()
      if (length(index) != 1L || is.na(index) || !index %in% seq_along(trial$plot$layers))
        stop("Select a layer to delete.")
      layer <- trial$plot$layers[[index]]
      is_shape <- inherits(layer$geom, "GeomFigureShape")
      label <- if (is_shape) tools::toTitleCase(shape_edit_name(layer$geom_params$annotation)) else "Layer"
      key <- if (is_shape) paste0("element/", shape_layer_key(layer, index))
        else if (length(reference_line_info(layer))) reference_move_key(layer, index)
      edits <- offsets()
      if (length(key))
        key <- c(key, names(edits)[startsWith(as.character(names(edits)), annotation_panel_prefix(key))])
      removed <- edits[intersect(key, names(edits))]
      previous <- previous_offsets()
      removed_previous <- previous[intersect(key, names(previous))]
      edits <- edits[!names(edits) %in% key]
      trial$element_edits <- trial$element_edits[!names(trial$element_edits) %in% key]
      trial$plot$layers[[index]] <- NULL
      tick_preview(trial)
      history[[length(history) + 1L]] <<- list(index = index, layer = layer, offsets = removed,
                                              previous = removed_previous, label = label)
      offsets(edits)
      previous_offsets(previous[!names(previous) %in% key])
      edited_plot(trial$plot)
      history_status(paste(label, "deleted."))
      clear_selection()
    }
    bind_example_controls(input, output, session, switch_figure)
    bind_theme_controls(input, output, edited_plot, current, function() document()$project$plot$theme)
    bind_grid_controls(input, output, edited_plot, current, grid_visibility)
    bind_custom_theme(input, output, edited_plot, current, grid_visibility)
    bind_layer_controls(input, output, session, edited_plot, current, preview, delete_layer)
    bind_element_inspector(input, output, session, edited_plot, current, preview, title_override, palettes,
                           delete_layer, offsets)
    output$preview <- shiny::renderUI(preview()$canvas)
    output$issues <- shiny::renderText(paste(preview()$issues, collapse = "\n"))
    shiny::observeEvent(input$movements, {
      on.exit(session$sendCustomMessage("movements-applied", TRUE))
      keys <- preview()$move_keys
      edits <- offsets()
      previous <- previous_offsets()
      for (event in jsonlite::fromJSON(input$movements, simplifyVector = FALSE)) {
        if (isTRUE(event$undo)) {
          n <- length(history)
          if (!n) next
          last <- history[[n]]
          if (!is.null(last$layer)) {
            plot <- edited_plot()
            plot$layers <- append(plot$layers, list(last$layer), after = last$index - 1L)
            edited_plot(plot)
            if (length(last$offsets)) edits[names(last$offsets)] <- last$offsets
            if (length(last$previous)) previous[names(last$previous)] <- last$previous
            history_status(paste(last$label, "restored."))
            clear_selection()
          } else {
            edits[[last$key]] <- last$offset
            previous[[last$key]] <- last$previous
          }
          history[[n]] <<- NULL
          next
        }
        if (!is.character(event$key) || length(event$key) != 1L ||
            !event$key %in% keys) next
        delta <- c(event$dx, event$dy)
        if (!is.numeric(delta) || length(delta) != 2L || !all(is.finite(delta)) ||
            all(delta == 0)) next
        old <- edits[[event$key]]
        history[[length(history) + 1L]] <<- list(key = event$key, offset = old, previous = previous[[event$key]])
        if (grepl("^element/(panel|shape)/", event$key)) previous[[event$key]] <- old
        if (is.null(old)) old <- c(0, 0)
        edits[[event$key]] <- old + delta
      }
      offsets(edits)
      previous_offsets(previous)
    })
    shiny::observeEvent(input$done, {
      shiny::req(preview())
      shiny::req(is.finite(input$nudge), input$nudge > 0)
      result <- if (!is.null(composition) &&
                    (document()$key %in% plot_keys || isTRUE(input$show_composition))) combined_project()
        else active_project()
      shiny::stopApp(result)
    }, priority = -100)
  }
  shiny::shinyApp(ui, server)
}
