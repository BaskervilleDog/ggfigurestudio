layer_geom_options <- function() {
  c("Points" = "point", "Lines" = "line", "Steps" = "step", "Tiles" = "tile",
    "Columns" = "col", "Histogram" = "histogram", "Density" = "density",
    "Boxplot" = "boxplot", "Violin" = "violin", "Ribbon" = "ribbon",
    "Text" = "text", "Boxed labels" = "label", "Segments" = "segment")
}

layer_choices <- function(plot) {
  layers <- plot$layers
  if (!length(layers)) return(character())
  labels <- vapply(layers, function(layer) {
    if (inherits(layer$geom, "GeomFigureShape")) return(shape_title(layer$geom_params$annotation))
    geom <- sub("^Geom", "", class(layer$geom)[1L])
    stat <- sub("^Stat", "", class(layer$stat)[1L])
    sprintf("%s (%s)", geom, stat)
  }, character(1))
  ids <- seq_along(layers)
  stats::setNames(as.character(ids), paste0(ids, ". ", labels))
}

layer_controls <- function(plot, all_annotation_facets = TRUE) {
  shiny::tagList(
    editor_select("layer_operation", "Operation", list(
      "Add annotation" = c("Add vertical line" = "vertical", "Add horizontal line" = "horizontal",
                           "Add box" = "box", "Add circle or ellipse" = "circle",
                           "Add text" = "text", "Add label" = "label"),
      "Manage existing layer" = c("Change layer geometry" = "change", "Delete layer" = "delete")
    ), width = "100%"),
    shiny::tags$details(class = "editor-details",
      shiny::tags$summary("Facet styling"),
      shiny::checkboxInput("annotation_all_facets", "Apply shape and line styles to all facets",
                           all_annotation_facets),
      shiny::p(class = "editor-help",
        "This sets the default style scope. To edit particular copies, click the annotation and choose its scope in Selection.")),
    shiny::conditionalPanel("input.layer_operation !== 'change' && input.layer_operation !== 'delete'",
      shiny::uiOutput("annotation_add_scope")),
    shiny::conditionalPanel("input.layer_operation === 'change' || input.layer_operation === 'delete'",
      editor_group("Target layer", editor_select("layer_index", "Layer", layer_choices(plot), width = "100%"),
        description = "Click a mark in the preview to preselect its layer, or choose one here.")
    ),
    shiny::conditionalPanel("input.layer_operation === 'change'",
      editor_group("Geometry",
        editor_select("layer_geom", "New geometry", layer_geom_options(), selected = "tile", width = "100%"),
        shiny::checkboxInput("keep_layer_stat", "Keep current statistic and position adjustment", TRUE),
        shiny::p(class = "editor-help", "Clear this option to use the new geometry's defaults."),
        shiny::conditionalPanel("input.layer_geom === 'tile'",
          editor_fields(shiny::textInput("tile_width", "Tile width", placeholder = "Automatic"),
                        shiny::textInput("tile_height", "Tile height", placeholder = "Automatic")),
          shiny::checkboxInput("tile_map_fill", "Use mapped colour for tile fill", FALSE)),
        description = "The new geometry needs compatible data mappings.")
    ),
    shiny::conditionalPanel("input.layer_operation === 'delete'",
      shiny::p(class = "editor-help", "This removes the entire layer from every facet. Ctrl+Z restores it.")
    ),
    shiny::conditionalPanel("input.layer_operation === 'box' || input.layer_operation === 'circle'",
      shape_fields("new_shape_")
    ),
    shiny::conditionalPanel("input.layer_operation === 'text'", shape_fields("new_text_", type = "text")),
    shiny::conditionalPanel("input.layer_operation === 'label'", shape_fields("new_label_", type = "label")),
    shiny::conditionalPanel("input.layer_operation === 'vertical' || input.layer_operation === 'horizontal'",
      editor_group("Position",
        shiny::textInput("reference_at", "Position on the axis", placeholder = "e.g. 5"),
        description = "Use an x value for a vertical line and a y value for a horizontal line."),
      editor_group("Line appearance",
        editor_fields(shiny::textInput("reference_colour", "Line colour", "#B33A49"),
                      shiny::numericInput("reference_width", "Line width (mm)", 0.7, min = 0)),
        editor_select("reference_type", "Line type",
          c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dashed"))
    ),
    editor_actions(shiny::actionButton("apply_layer", "Apply layer change", class = "btn-primary")),
    editor_status("layer_status")
  )
}

reference_axis <- function(geom) {
  if (inherits(geom, "GeomVline")) return("x")
  if (inherits(geom, "GeomHline")) return("y")
  NULL
}

reference_position <- function(plot, axis, text, panel = 1L) {
  scale <- ggplot2::ggplot_build(scope_annotation_facets(plot))$layout$get_scales(panel)[[axis]]
  parse_axis_position(scale, text)
}

parse_axis_position <- function(scale, text) {
  text <- trimws(text)
  if (!nzchar(text)) stop("Enter a position on the axis.")
  if (inherits(scale, "ScaleContinuousDate")) {
    value <- as.Date(text, format = "%Y-%m-%d")
    if (!is.na(value)) return(value)
    stop("Enter the date as YYYY-MM-DD.")
  }
  if (inherits(scale, "ScaleContinuousDatetime")) {
    value <- as.POSIXct(text, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d"),
                        optional = TRUE)
    if (!is.na(value)) return(value)
    stop("Enter the date and time as YYYY-MM-DD HH:MM:SS (UTC).")
  }
  if (!is.null(scale) && scale$is_discrete() && text %in% as.character(scale$get_limits()))
    return(as.numeric(scale$map(text)))
  value <- suppressWarnings(as.numeric(text))
  if (!is.finite(value)) stop("Enter a number or a category shown on the axis.")
  value
}

add_reference_line <- function(plot, axis, at, colour, width, type, panel = 1L) {
  value <- reference_position(plot, axis, at, panel)
  colour <- mark_value("colour", colour)
  width <- mark_value("linewidth", width)
  line <- if (axis == "x") {
    ggplot2::geom_vline(xintercept = value, colour = colour, linewidth = width, linetype = type)
  } else {
    ggplot2::geom_hline(yintercept = value, colour = colour, linewidth = width, linetype = type)
  }
  line$figure_reference_id <- basename(tempfile("reference-"))
  plot + line
}

reference_segment_coords <- function(panel, params, coord, axis) {
  ranges <- coord$backtransform_range(params)
  other <- if (axis == "x") "y" else "x"
  panel[[axis]] <- panel[[paste0(axis, "intercept")]]
  panel[[paste0(axis, "end")]] <- panel[[axis]]
  panel[[other]] <- ranges[[other]][1L]
  panel[[paste0(other, "end")]] <- ranges[[other]][2L]
  panel
}

reference_line_info <- function(layer) {
  axis <- reference_axis(layer$geom)
  if (is.null(axis) || !is.data.frame(layer$data) || nrow(layer$data) != 1L) return(NULL)
  column <- paste0(axis, "intercept")
  mapping <- layer$mapping[[column]]
  if (!column %in% names(layer$data) || !identical(describe_aes(mapping), column)) return(NULL)
  list(axis = axis, value = as.character(layer$data[[column]][1L]))
}

move_reference_line <- function(plot, index, reference, text) {
  layer <- plot$layers[[index]]
  copy <- rlang::env_clone(layer)
  attributes(copy) <- attributes(layer)
  data <- layer$data
  data[[paste0(reference$axis, "intercept")]] <- reference_position(plot, reference$axis, text)
  copy$data <- data
  plot$layers[[index]] <- copy
  plot
}

replace_layer_geom <- function(plot, index, geom, keep_stat = TRUE, tile_size = list(), map_fill = FALSE) {
  if (!geom %in% layer_geom_options()) stop("Select a supported geometry.")
  if (length(index) != 1L || is.na(index) || !index %in% seq_along(plot$layers))
    stop("Select a layer.")
  original <- plot$layers[[index]]
  constructor <- getExportedValue("ggplot2", paste0("geom_", geom))
  target <- constructor()
  if (identical(class(target$geom)[1L], class(original$geom)[1L]) &&
      keep_stat && !length(tile_size) && !map_fill) return(plot)
  if (keep_stat) {
    target$stat <- original$stat
    target$stat_params <- original$stat_params
    target$position <- original$position
  }
  supported <- union(target$geom$aesthetics(), target$stat$aesthetics())
  target$mapping <- original$mapping[intersect(names(original$mapping), supported)]
  target$aes_params <- original$aes_params[intersect(names(original$aes_params), target$geom$aesthetics())]
  common_params <- intersect(names(original$geom_params), target$geom$parameters(extra = TRUE))
  target$geom_params[common_params] <- original$geom_params[common_params]
  target$data <- original$data
  target$inherit.aes <- original$inherit.aes
  target$show.legend <- original$show.legend
  target$layout <- original$layout
  if (geom == "tile") {
    target$aes_params[names(tile_size)] <- tile_size
    if (map_fill) {
      colour <- original$mapping[["colour"]]
      if (is.null(colour) && isTRUE(original$inherit.aes)) colour <- plot$mapping[["colour"]]
      if (is.null(colour) || "colour" %in% names(original$aes_params))
        stop("This layer needs a mapped colour to reuse it for tile fill.")
      if (is.null(target$mapping)) target$mapping <- ggplot2::aes()
      target$mapping[["fill"]] <- colour
      target$aes_params[["fill"]] <- NULL
    }
  }
  plot$layers[[index]] <- target
  plot
}

bind_layer_controls <- function(input, output, session, edited_plot, current, preview, delete_layer) {
  status <- shiny::reactiveVal("")
  output$layer_status <- shiny::renderText(status())
  output$annotation_add_scope <- shiny::renderUI({
    choices <- preview()$annotation_facets
    if (!length(choices)) return(NULL)
    editor_group("Placement",
      editor_select("annotation_add_facets", "Add to facets", choices, multiple = TRUE,
                    selected = intersect(shiny::isolate(input$annotation_add_facets), choices), width = "100%"),
      description = "Choose one or more facets. Leave empty to add a copy in every facet.")
  })
  shiny::observeEvent(edited_plot(), {
    choices <- layer_choices(edited_plot())
    previous <- shiny::isolate(input$layer_index)
    selected <- if (length(previous) && previous %in% choices) previous else head(choices, 1L)
    shiny::updateSelectInput(session, "layer_index", choices = choices, selected = selected)
  })
  shiny::observeEvent(input$inspect_element, {
    mark <- preview()$marks[[input$inspect_element]]
    if (length(mark$layer))
      shiny::updateSelectInput(session, "layer_index", selected = as.character(mark$layer))
  })
  shiny::observeEvent(input$apply_layer, {
    tryCatch({
      plot <- edited_plot()
      operation <- shiny::req(input$layer_operation)
      annotation_operation <- operation %in% c("box", "circle", "text", "label")
      targets <- if (annotation_operation || operation %in% c("vertical", "horizontal"))
        input$annotation_add_facets
      choices <- if (length(targets)) preview()$annotation_facets
      if (length(targets) && !all(targets %in% choices)) stop("Choose facets from the current figure.")
      panel <- if (length(targets)) match(targets[1L], choices) else 1L
      if (operation == "delete") {
        delete_layer(as.integer(input$layer_index))
        status("")
        return()
      }
      if (operation == "change") {
        size <- list()
        if (identical(input$layer_geom, "tile")) {
          for (field in c("width", "height")) {
            text <- trimws(input[[paste0("tile_", field)]])
            if (!length(text) || !nzchar(text)) next
            value <- mark_value(field, text)
            if (value <= 0) stop("Tile dimensions must be greater than zero.")
            size[[field]] <- value
          }
        }
        candidate <- replace_layer_geom(plot, as.integer(input$layer_index), input$layer_geom,
          keep_stat = isTRUE(input$keep_layer_stat), tile_size = size,
          map_fill = isTRUE(input$tile_map_fill))
        message <- "Layer geometry changed."
      } else if (annotation_operation) {
        prefix <- switch(operation, text = "new_text_", label = "new_label_", "new_shape_")
        annotation <- shape_spec(input, prefix, operation)
        candidate <- shape_plot(plot, annotation, panel = panel)
        message <- paste(shape_title(annotation), "added.")
      } else {
        axis <- if (operation == "vertical") "x" else "y"
        candidate <- add_reference_line(plot, axis, input$reference_at, input$reference_colour,
                                         input$reference_width, input$reference_type, panel = panel)
        message <- "Reference line added."
      }
      if (length(targets) && operation != "change") {
        index <- length(candidate$layers)
        candidate$layers[[index]]$layout <- structure(which(choices %in% targets), figure_facets = targets)
        position <- if (annotation_operation) annotation[c("x", "y")] else
          stats::setNames(list(input$reference_at), axis)
        candidate$layers[[index]]$figure_facet_positions <-
          stats::setNames(rep(list(position), length(targets)), targets)
      }
      trial <- current()
      trial$plot <- candidate
      if (annotation_operation) check_shape_preview(trial, length(candidate$layers))
      else tick_preview(trial)
      edited_plot(candidate)
      status(message)
    }, error = function(e) status(paste("Layer change could not be applied.", conditionMessage(e))))
  })
}
