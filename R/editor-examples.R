editor_examples <- function() {
  list(
    "General plots" = list(catalog = gallery_examples(), build = gallery_example),
    "Bioinformatics" = list(catalog = bioinfo_examples(), build = bioinfo_example),
    "Plant breeding and quantitative genetics" = list(catalog = breeding_examples(), build = breeding_example),
    "Maps" = list(catalog = map_examples(), build = map_example)
  )
}

example_controls <- function() {
  groups <- editor_examples()
  catalog <- groups[[1L]]$catalog
  shiny::tagList(
    shiny::h2("Example gallery"),
    shiny::p(class = "editor-help",
      "Open a built-in plot and try its editing controls. Edits to each figure are kept while this editor is open."),
    editor_select("example_gallery", "Gallery", names(groups), width = "100%"),
    editor_select("example_id", "Example", example_choices(catalog), width = "100%"),
    shiny::uiOutput("example_details"),
    editor_actions(
      shiny::actionButton("open_example", "Open example", class = "btn-primary"),
      shiny::actionButton("restore_original", "Back to starting figure")),
    editor_status("example_status")
  )
}

example_choices <- function(catalog) {
  choices <- stats::setNames(catalog$id, catalog$plot)
  if (is.null(catalog$category)) return(choices)
  split(choices, factor(catalog$category, levels = unique(catalog$category)))
}

bind_example_controls <- function(input, output, session, switch_figure) {
  groups <- editor_examples()
  gallery <- shiny::reactive({
    name <- shiny::req(input$example_gallery)
    shiny::req(name %in% names(groups))
    groups[[name]]
  })
  selected <- shiny::reactive({
    catalog <- gallery()$catalog
    row <- match(input$example_id, catalog$id)
    shiny::req(length(row), !is.na(row))
    catalog[row, , drop = FALSE]
  })
  status <- shiny::reactiveVal("")
  output$example_status <- shiny::renderText(status())
  output$example_details <- shiny::renderUI({
    row <- selected()
    packages <- if (length(row$packages))
      setdiff(strsplit(row$packages, ", ", fixed = TRUE)[[1L]], "ggplot2")
    shiny::tagList(
      shiny::p(class = "editor-help", row$try_editing),
      if (length(packages)) shiny::p(class = "editor-help", paste("Requires", paste(packages, collapse = ", ")))
    )
  })
  shiny::observeEvent(input$example_gallery, {
    choices <- example_choices(gallery()$catalog)
    shiny::updateSelectInput(session, "example_id", choices = choices, selected = gallery()$catalog$id[1L])
    status("")
  })
  shiny::observeEvent(input$open_example, {
    row <- selected()
    tryCatch({
      switch_figure(paste(input$example_gallery, row$id, sep = "/"), row$plot,
                    function() gallery()$build(row$id))
      status(paste("Showing", row$plot))
    }, error = function(e) status(paste("Could not open example.", conditionMessage(e))))
  })
  shiny::observeEvent(input$restore_original, {
    switch_figure("original")
    status("Showing the starting figure.")
  })
}
