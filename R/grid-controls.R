grid_elements <- function(type) {
  parent <- paste0("panel.grid.", type)
  c(parent, paste0(parent, c(".x", ".y")))
}

apply_grid_visibility <- function(plot, visibility) {
  if (!length(visibility)) return(plot)
  theme <- ggplot2::theme_get() + plot$theme
  settings <- list()
  for (type in names(visibility)) {
    elements <- grid_elements(type)
    parent <- ggplot2::calc_element(elements[1L], theme)
    if (!inherits(parent, "element_line"))
      parent <- ggplot2::calc_element(elements[1L], ggplot2::theme_light())
    for (element in elements) {
      line <- ggplot2::calc_element(element, theme)
      if (!inherits(line, "element_line")) line <- parent
      line$inherit.blank <- FALSE
      settings[[element]] <- if (isTRUE(visibility[[type]])) line else ggplot2::element_blank()
    }
  }
  plot + do.call(ggplot2::theme, settings)
}

grid_visibility_values <- function(plot, visibility) {
  theme <- ggplot2::theme_get() + plot$theme
  types <- if (inherits(plot$coordinates, "CoordSf")) "major" else c("major", "minor")
  stats::setNames(vapply(types, function(type) {
    if (!is.null(visibility[[type]])) return(visibility[[type]])
    elements <- grid_elements(type)
    elements <- if (length(types) == 1L) elements[1L] else elements[-1L]
    any(vapply(elements, function(element) {
      inherits(ggplot2::calc_element(element, theme), "element_line")
    }, logical(1)))
  }, logical(1)), types)
}

bind_grid_controls <- function(input, output, edited_plot, current, grid_visibility) {
  settings <- shiny::reactive(grid_visibility_values(edited_plot(), grid_visibility()))
  output$grid_controls <- shiny::renderUI({
    values <- settings()
    editor_group("Grid lines",
      lapply(names(values), function(type) {
        shiny::checkboxInput(paste0("grid_", type), paste("Show", type, "grid lines"), values[[type]])
      }),
      editor_actions(shiny::actionButton("apply_grid", "Apply grid settings")),
      editor_status("grid_status"),
      description = "Show or hide the guides across the plot panels.")
  })
  status <- shiny::reactiveVal("")
  output$grid_status <- shiny::renderText(status())
  shiny::observeEvent(input$apply_grid, {
    values <- settings()
    updated <- grid_visibility()
    for (type in names(values)) {
      value <- input[[paste0("grid_", type)]]
      if (is.logical(value) && length(value) == 1L && !identical(value, values[[type]]))
        updated[[type]] <- value
    }
    tryCatch({
      trial <- current()
      trial$grid_visibility <- updated
      tick_preview(trial)
      grid_visibility(updated)
      status("Grid settings applied.")
    }, error = function(e) status(paste("Could not apply grid settings.", conditionMessage(e))))
  })
}
