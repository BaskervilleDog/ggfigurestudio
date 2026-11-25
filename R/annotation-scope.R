annotation_copy_choices <- function(layer, choices) {
  restricted <- attr(layer$layout, "figure_facets")
  if (length(restricted)) choices[choices %in% restricted] else choices
}

annotation_edit_controls <- function(mark, choices, mode, panels) {
  if (!isTRUE(mark$faceted)) return(NULL)
  if (!length(mode)) mode <- "default"
  panels <- intersect(panels, choices)
  if (!length(panels)) panels <- mark$panel_suffix
  editor_group("Facet scope",
    editor_select("annotation_edit_mode", "Apply edits to",
      c("Default scope" = "default", "This facet" = "this",
        "Selected facets" = "selected", "All copies" = "all"), selected = mode, width = "100%"),
    shiny::conditionalPanel("input.annotation_edit_mode === 'selected'",
      editor_select("annotation_edit_facets", "Facets to edit", choices, multiple = TRUE,
                    selected = panels, width = "100%")),
    description = "Explicit scopes apply to both position and style. The default edits this position and uses the Layers style setting.")
}

annotation_edit_scope <- function(mark, layer, choices, input) {
  mode <- input$annotation_edit_mode
  if (identical(mode, "all"))
    return(list(panels = unname(annotation_copy_choices(layer, choices)), shared = TRUE))
  if (identical(mode, "selected")) {
    panels <- input$annotation_edit_facets
    if (!length(panels) || !all(panels %in% annotation_copy_choices(layer, choices)))
      stop("Choose one or more facets containing this annotation.")
    return(list(panels = panels, shared = FALSE))
  }
  list(panels = mark$panel_suffix,
       shared = !identical(mode, "this") && !identical(input$annotation_all_facets, FALSE))
}
